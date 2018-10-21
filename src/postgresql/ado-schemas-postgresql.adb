-----------------------------------------------------------------------
--  ado-schemas-postgresql -- Postgresql Database Schemas
--  Copyright (C) 2018 Stephane Carrez
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
package body ADO.Schemas.Postgresql is

   use ADO.Statements;

   procedure Load_Table_Schema (C        : in ADO.Drivers.Connections.Database_Connection'Class;
                                Database : in String;
                                Table    : in Table_Definition);

   procedure Load_Table_Keys (C        : in ADO.Drivers.Connections.Database_Connection'Class;
                              Database : in String;
                              Table    : in Table_Definition);

   function String_To_Type (Value : in String) return Column_Type;

   function String_To_Type (Value : in String) return Column_Type is
   begin
      if Value = "date" then
         return T_DATE;
      elsif Value = "timestamp without time zone" then
         return T_DATE_TIME;
      elsif Value = "timestamp with time zone" then
         return T_DATE_TIME;
      elsif Value = "integer" then
         return T_INTEGER;
      elsif Value = "bigint" then
         return T_LONG_INTEGER;
      elsif Value = "bytea" then
         return T_BLOB;
      elsif Value = "character varying" then
         return T_VARCHAR;
      end if;
      return T_UNKNOWN;
   end String_To_Type;

   --  ------------------------------
   --  Load the table definition
   --  ------------------------------
   procedure Load_Table_Schema (C        : in ADO.Drivers.Connections.Database_Connection'Class;
                                Database : in String;
                                Table    : in Table_Definition) is
      Name  : constant String := Get_Name (Table);
      SQL   : constant String
        := "SELECT column_name, column_default, data_type, is_nullable, "
          & "character_maximum_length, collation_name FROM information_schema.columns "
          & "WHERE table_catalog = ? AND table_name = ?";
      Stmt  : Query_Statement
        := Create.Create_Statement (C.Create_Statement (SQL));
      Last  : Column_Definition := null;
      Col   : Column_Definition;
      Value : Unbounded_String;
   begin
      Stmt.Add_Param (Database);
      Stmt.Add_Param (Name);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         Col           := new Column;
         Col.Name      := Stmt.Get_Unbounded_String (0);
         if not Stmt.Is_Null (5) then
            Col.Collation := Stmt.Get_Unbounded_String (5);
         end if;
         if not Stmt.Is_Null (1) then
            Col.Default   := Stmt.Get_Unbounded_String (1);
         end if;
         if Last /= null then
            Last.Next_Column := Col;
         else
            Table.First_Column := Col;
         end if;

         Value := Stmt.Get_Unbounded_String (2);
         Col.Col_Type := String_To_Type (To_String (Value));

         Value := Stmt.Get_Unbounded_String (3);
         Col.Is_Null := Value = "YES";

         Last := Col;
         Stmt.Next;
      end loop;
   end Load_Table_Schema;

   --  ------------------------------
   --  Load the table definition
   --  ------------------------------
   procedure Load_Table_Keys (C        : in ADO.Drivers.Connections.Database_Connection'Class;
                              Database : in String;
                              Table    : in Table_Definition) is
      Name  : constant String := Get_Name (Table);
      SQL   : constant String
        := "SELECT column_name FROM"
        & " information_schema.table_constraints tc, "
        & " information_schema.key_column_usage kc "
        & "WHERE tc.constraint_type = 'PRIMARY KEY' "
        & " AND kc.table_name = tc.table_name "
        & " AND kc.table_schema = tc.table_schema "
        & " AND kc.constraint_name = tc.constraint_name "
        & " AND tc.table_catalog = ? and tc.table_name = ?";
      Stmt  : Query_Statement
        := Create.Create_Statement (C.Create_Statement (SQL));
      Col   : Column_Definition;
   begin
      Stmt.Add_Param (Database);
      Stmt.Add_Param (Name);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         declare
            Col_Name : constant String := Stmt.Get_String (0);
         begin
            Col := Find_Column (Table, Col_Name);
            if Col /= null then
               Col.Is_Primary := True;
            end if;
         end;

         Stmt.Next;
      end loop;
   end Load_Table_Keys;

   --  ------------------------------
   --  Load the database schema
   --  ------------------------------
   procedure Load_Schema (C        : in ADO.Drivers.Connections.Database_Connection'Class;
                          Schema   : out Schema_Definition;
                          Database : in String) is
      SQL   : constant String
        := "SELECT tablename FROM pg_catalog.pg_tables "
          & "WHERE schemaname != 'pg_catalog' AND schemaname != 'information_schema'";
      Stmt  : Query_Statement
        := Create.Create_Statement (C.Create_Statement (SQL));
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
         Load_Table_Schema (C, Database, Table);
         Load_Table_Keys (C, Database, Table);
         Last := Table;
         Stmt.Next;
      end loop;
   end Load_Schema;

end ADO.Schemas.Postgresql;
