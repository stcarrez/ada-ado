-----------------------------------------------------------------------
--  ado.schemas -- Database Schemas
--  Copyright (C) 2009, 2010, 2011, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with ADO.Statements;
with ADO.Statements.Create;
with ADO.SQL;
with Ada.Strings.Fixed;
package body ADO.Schemas.Mysql is

   use ADO.Statements;

   procedure Load_Table_Schema (C : in ADO.Drivers.Connections.Database_Connection'Class;
                                Table : in Table_Definition);

   function String_To_Type (Value : in String) return Column_Type;
   function String_To_Length (Value : in String) return Natural;

   function String_To_Type (Value : in String) return Column_Type is
      Pos : Natural;
   begin
      if Value = "date" then
         return T_DATE;
      elsif Value = "datetime" then
         return T_DATE_TIME;
      elsif Value = "int(11)" then
         return T_INTEGER;
      elsif Value = "bigint(20)" then
         return T_LONG_INTEGER;
      elsif Value = "tinyint(4)" then
         return T_TINYINT;
      elsif Value = "smallint(6)" then
         return T_SMALLINT;
      elsif Value = "longblob" then
         return T_BLOB;
      end if;
      Pos := Ada.Strings.Fixed.Index (Value, "(");
      if Pos > 0 then
         declare
            Name : constant String := Value (Value'First .. Pos - 1);
         begin
            if Name = "varchar" then
               return T_VARCHAR;
            elsif Name = "decimal" then
               return T_FLOAT;
            elsif Name = "int" then
               return T_INTEGER;
            elsif Name = "bigint" then
               return T_LONG_INTEGER;
            else
               return T_UNKNOWN;
            end if;
         end;
      end if;
      return T_UNKNOWN;
   end String_To_Type;

   function String_To_Length (Value : in String) return Natural is
      First : Natural;
      Last  : Natural;
   begin
      First := Ada.Strings.Fixed.Index (Value, "(");
      if First = 0 then
         return 0;
      end if;
      Last := Ada.Strings.Fixed.Index (Value, ")");
      if Last < First then
         return 0;
      end if;
      return Natural'Value (Value (First + 1 .. Last - 1));

   exception
      when Constraint_Error =>
         return 0;
   end String_To_Length;

   --  ------------------------------
   --  Load the table definition
   --  ------------------------------
   procedure Load_Table_Schema (C : in ADO.Drivers.Connections.Database_Connection'Class;
                                Table : in Table_Definition) is
      Name : constant String := Get_Name (Table);
      Stmt : Query_Statement
        := Create.Create_Statement (C.Create_Statement ("show full columns from "));
      Last : Column_Definition := null;
      Col  : Column_Definition;
      Value : Unbounded_String;
      Query : constant ADO.SQL.Query_Access := Stmt.Get_Query;
   begin
      ADO.SQL.Append_Name (Target => Query.SQL, Name => Name);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         Col           := new Column;
         Col.Name      := Stmt.Get_Unbounded_String (0);
         if not Stmt.Is_Null (2) then
            Col.Collation := Stmt.Get_Unbounded_String (2);
         end if;
         if not Stmt.Is_Null (5) then
            Col.Default   := Stmt.Get_Unbounded_String (5);
         end if;
         if Last /= null then
            Last.Next_Column := Col;
         else
            Table.First_Column := Col;
         end if;

         Value := Stmt.Get_Unbounded_String (1);
         Col.Col_Type := String_To_Type (To_String (Value));
         Col.Size := String_To_Length (To_String (Value));

         Value := Stmt.Get_Unbounded_String (3);
         Col.Is_Null := Value = "YES";

         Value := Stmt.Get_Unbounded_String (4);
         Col.Is_Primary := Value = "PRI";

         Last := Col;
         Stmt.Next;
      end loop;
   end Load_Table_Schema;

   --  ------------------------------
   --  Load the database schema
   --  ------------------------------
   procedure Load_Schema (C      : in ADO.Drivers.Connections.Database_Connection'Class;
                          Schema : out Schema_Definition) is
      Stmt  : Query_Statement
        := Create.Create_Statement (C.Create_Statement ("show tables"));
      Table : Table_Definition;
      Last  : Table_Definition := null;
   begin
      Schema.Schema := new ADO.Schemas.Schema;
      Stmt.Execute;

      while Stmt.Has_Elements loop
         Table      := new ADO.Schemas.Table;
         Table.Name := Stmt.Get_Unbounded_String (0);
         if Last /= null then
            Last.Next_Table := Table;
         else
            Schema.Schema.First_Table := Table;
         end if;
         Load_Table_Schema (C, Table);
         Last := Table;
         Stmt.Next;
      end loop;
   end Load_Schema;

end ADO.Schemas.Mysql;
