-----------------------------------------------------------------------
--  pschema - Print the database schema
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with ADO;
with ADO.Schemas.Mysql;
with ADO.Drivers;
with ADO.Sessions;
with ADO.Sessions.Factory;
with ADO.Schemas;

with Ada.Text_IO;

with Util.Log.Loggers;

procedure Pschema is

   use ADO;
   use Ada;
   use ADO.Schemas;

   Factory    : ADO.Sessions.Factory.Session_Factory;

begin
   Util.Log.Loggers.Initialize ("samples.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("samples.properties");

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Factory.Create (ADO.Drivers.Get_Config ("ado.database"));

   declare
      DB     : constant ADO.Sessions.Master_Session := Factory.Get_Master_Session;
      Schema : ADO.Schemas.Schema_Definition;
      Iter   : Table_Cursor;
   begin
      ADO.Schemas.Mysql.Load_Schema (DB.Get_Connection, Schema);

      --  Dump the database schema using SQL create table forms.
      Iter := Get_Tables (Schema);
      while Has_Element (Iter) loop
         declare
            Table      : constant Table_Definition := Element (Iter);
            Table_Iter : Column_Cursor := Get_Columns (Table);
         begin
            Ada.Text_IO.Put_Line ("create table " & Get_Name (Table) & " (");
            while Has_Element (Table_Iter) loop
               declare
                  Col : constant Column_Definition := Element (Table_Iter);
               begin
                  Ada.Text_IO.Put ("  ");
                  Ada.Text_IO.Put (Get_Name (Col));
                  Ada.Text_IO.Put (" ");
                  Ada.Text_IO.Put (Column_Type'Image (Get_Type (Col)));
                  if not Is_Null (Col) then
                     Ada.Text_IO.Put (" not null");
                  end if;
                  Ada.Text_IO.Put_Line (",");
               end;
               Next (Table_Iter);
            end loop;
            Ada.Text_IO.Put_Line (");");
         end;
         Next (Iter);
      end loop;
   end;
end Pschema;
