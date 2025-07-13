-----------------------------------------------------------------------
--  pschema - Print the database schema
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO;
with ADO.Sessions;
with ADO.Sessions.Factory;
with ADO.Schemas;
with ADO.Configs;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;

with Util.Strings.Transforms;
with Util.Properties;
with DB_Initialize;
procedure Pschema is

   use ADO;
   use ADO.Schemas;
   use Util.Strings.Transforms;

   function To_Model_Type (Kind : in ADO.Schemas.Column_Type) return String;

   function To_Model_Type (Kind : in ADO.Schemas.Column_Type) return String is
   begin
      case Kind is
         when T_BOOLEAN =>
            return "boolean";

         when T_TINYINT | T_SMALLINT | T_INTEGER =>
            return "integer";

         when T_LONG_INTEGER =>
            return "long";

         when T_TIME | T_DATE_TIME | T_TIMESTAMP =>
            return "datetime";

         when T_DATE =>
            return "date";

         when T_BLOB =>
            return "blob";

         when T_VARCHAR =>
            return "string";

         when others =>
            return "?";

      end case;
   end To_Model_Type;

   Factory    : ADO.Sessions.Factory.Session_Factory;
   Props      : Util.Properties.Manager;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage: pschema connection");
      Ada.Text_IO.Put_Line ("Example: pschema mysql://localhost:3306/test");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   --  Initialize the database drivers.
   Props.Set (ADO.Configs.NO_ENTITY_LOAD, "true");
   DB_Initialize (Props);

   --  Initialize the session factory to connect to the
   --  database defined from command line.
   Factory.Create (Ada.Command_Line.Argument (1));

   declare
      DB     : constant ADO.Sessions.Master_Session := Factory.Get_Master_Session;
      Schema : ADO.Schemas.Schema_Definition;
      Iter   : Table_Cursor;
   begin
      DB.Load_Schema (Schema);

      --  Dump the database schema using SQL create table forms.
      Iter := Get_Tables (Schema);
      while Has_Element (Iter) loop
         declare
            Table      : constant Table_Definition := Element (Iter);
            Table_Iter : Column_Cursor := Get_Columns (Table);
         begin
            --  Ada.Text_IO.Put_Line ("create table " & Get_Name (Table) & " (");
            Ada.Text_IO.Put_Line (Get_Name (Table) & ":");
            Ada.Text_IO.Put_Line ("  type: entity");
            Ada.Text_IO.Put ("  table: ");
            Ada.Text_IO.Put_Line (Get_Name (Table));
            Ada.Text_IO.Put_Line ("  id:");
            while Has_Element (Table_Iter) loop
               declare
                  Col : constant Column_Definition := Element (Table_Iter);
               begin
                  if Is_Primary (Col) then
                     Ada.Text_IO.Put ("    ");
                     Ada.Text_IO.Put (To_Lower_Case (Get_Name (Col)));
                     Ada.Text_IO.Put_Line (":");
                     Ada.Text_IO.Put ("      type: ");
                     if Get_Type (Col) in T_INTEGER | T_LONG_INTEGER then
                        Ada.Text_IO.Put_Line ("identifier");
                     else
                        Ada.Text_IO.Put_Line (To_Model_Type (Get_Type (Col)));
                        Ada.Text_IO.Put ("      length:");
                        Ada.Text_IO.Put_Line (Natural'Image (Get_Size (Col)));
                     end if;
                     Ada.Text_IO.Put ("      column: ");
                     Ada.Text_IO.Put_Line (Get_Name (Col));
                     Ada.Text_IO.Put ("      not-null: ");
                     Ada.Text_IO.Put_Line ((if Is_Null (Col) then "true" else "false"));
                  end if;
               end;
               Next (Table_Iter);
            end loop;

            Ada.Text_IO.Put_Line ("  fields:");
            Table_Iter := Get_Columns (Table);
            while Has_Element (Table_Iter) loop
               declare
                  Col : constant Column_Definition := Element (Table_Iter);
               begin
                  if not Is_Primary (Col) then
                     Ada.Text_IO.Put ("    ");
                     Ada.Text_IO.Put (To_Lower_Case (Get_Name (Col)));
                     Ada.Text_IO.Put_Line (":");
                     Ada.Text_IO.Put ("      type: ");
                     Ada.Text_IO.Put_Line (To_Model_Type (Get_Type (Col)));
                     if Get_Size (Col) > 0 then
                        Ada.Text_IO.Put ("      length:");
                        Ada.Text_IO.Put_Line (Natural'Image (Get_Size (Col)));
                     end if;
                     Ada.Text_IO.Put ("      column: ");
                     Ada.Text_IO.Put_Line (Get_Name (Col));
                     Ada.Text_IO.Put ("      not-null: ");
                     Ada.Text_IO.Put_Line ((if Is_Null (Col) then "true" else "false"));
                     if Get_Default (Col)'Length > 0 then
                        Ada.Text_IO.Put ("      default: ");
                        Ada.Text_IO.Put_Line (Get_Default (Col));
                     end if;
                  end if;
               end;
               Next (Table_Iter);
            end loop;
            --  Ada.Text_IO.Put_Line (");");
         end;
         Next (Iter);
      end loop;
   end;

exception
   when E : ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));

end Pschema;
