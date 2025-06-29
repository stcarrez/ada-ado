-----------------------------------------------------------------------
--  ado-schemas -- Database Schemas
--  Copyright (C) 2015, 2018, 2019, 2020, 2021, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Statements;
with ADO.Statements.Create;
with Ada.Strings.Fixed;
with Util.Strings.Transforms;
package body ADO.Schemas.Sqlite is

   use ADO.Statements;

   procedure Load_Table_Schema (C : in ADO.Connections.Database_Connection'Class;
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
      elsif Value = "int" or else Value = "integer" then
         return T_INTEGER;
      elsif Value = "bigint" then
         return T_LONG_INTEGER;
      elsif Value = "tinyint" then
         return T_TINYINT;
      elsif Value = "smallint" then
         return T_SMALLINT;
      elsif Value = "float" then
         return T_FLOAT;
      elsif Value = "double" then
         return T_DOUBLE;
      elsif Value = "blob" then
         return T_BLOB;
      elsif Value = "text" then
         return T_VARCHAR;
      end if;
      Pos := Ada.Strings.Fixed.Index (Value, "(");
      if Pos > 0 then
         declare
            Name : constant String := Value (Value'First .. Pos - 1);
         begin
            if Name = "varchar" then
               return T_VARCHAR;
            elsif Name = "real" or else Name = "float" or else Name = "double" then
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
   procedure Load_Table_Schema (C : in ADO.Connections.Database_Connection'Class;
                                Table : in Table_Definition) is
      Name : constant String := Get_Name (Table);
      Stmt : Query_Statement
        := Create.Create_Statement (C.Create_Statement ("pragma table_info('" & Name & "')"));
      Last : Column_Definition := null;
      Col  : Column_Definition;
      Value : Unbounded_String;
   begin
      Stmt.Execute;
      while Stmt.Has_Elements loop
         Col           := new Column;
         Col.Name      := Stmt.Get_Unbounded_String (1);
         if not Stmt.Is_Null (4) then
            Col.Default   := Stmt.Get_Unbounded_String (4);
         end if;
         if Last /= null then
            Last.Next_Column := Col;
         else
            Table.First_Column := Col;
         end if;

         Value := Stmt.Get_Unbounded_String (2);
         declare
            Type_Name : constant String
              := Util.Strings.Transforms.To_Lower_Case (To_String (Value));
         begin
            Col.Col_Type := String_To_Type (Type_Name);
            Col.Size := String_To_Length (Type_Name);
         end;

         Value := Stmt.Get_Unbounded_String (3);
         Col.Is_Null := Value = "0";

         Value := Stmt.Get_Unbounded_String (5);
         Col.Is_Primary := Value = "1";

         Last := Col;
         Stmt.Next;
      end loop;
   end Load_Table_Schema;

   --  ------------------------------
   --  Load the database schema
   --  ------------------------------
   procedure Load_Schema (C      : in ADO.Connections.Database_Connection'Class;
                          Schema : out Schema_Definition) is
      Stmt  : Query_Statement
        := Create.Create_Statement (C.Create_Statement ("SELECT NAME FROM sqlite_master"));
      Table : Table_Definition;
      Last  : Table_Definition := null;
   begin
      Schema.Schema := new ADO.Schemas.Schema;
      Stmt.Execute;

      while Stmt.Has_Elements loop
         declare
            Name : constant String := Stmt.Get_String (0);
         begin
            if not Util.Strings.Starts_With (Name, "sqlite_") then
               Table      := new ADO.Schemas.Table;
               Table.Name := To_Unbounded_String (Name);
               if Last /= null then
                  Last.Next_Table := Table;
               else
                  Schema.Schema.First_Table := Table;
               end if;
               Load_Table_Schema (C, Table);
               Last := Table;
            end if;
         end;
         Stmt.Next;
      end loop;
   end Load_Schema;

end ADO.Schemas.Sqlite;
