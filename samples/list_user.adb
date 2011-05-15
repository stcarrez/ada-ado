-----------------------------------------------------------------------
--  List_User -- Example of queries
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with Samples.User.Model;

with ADO;
with ADO.Drivers;
with ADO.Queries;
with ADO.Sessions;
with ADO.Sessions.Factory;

with Util.Log.Loggers;

with Ada.Text_IO;
with Ada.Strings.Unbounded;
procedure List_User is

   use Ada.Strings.Unbounded;
   use Samples.User.Model;

   Factory    : ADO.Sessions.Factory.Session_Factory;

begin
   Util.Log.Loggers.Initialize ("samples.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("samples.properties");

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Factory.Create (ADO.Drivers.Get_Config ("ado.database"));

   declare
      DB      : ADO.Sessions.Session := Factory.Get_Session;
      Context : ADO.Queries.Context;
      Users   : Samples.User.Model.User_Info_Vector;
   begin
      --  Execute the 'Query_User_List' query (db/samples/user-list.xml, query 'user-list').
      Context.Set_Query (Samples.User.Model.Query_User_List);
      List (Object => Users, Session => DB, Context => Context);

      --  Print the list
      if Users.Is_Empty then
         Ada.Text_IO.Put_Line ("User info list is empty.");
         Ada.Text_IO.Put_Line ("Use the 'add_user' to add users in the table.");
      else
         declare
            Iter : Samples.User.Model.User_Info_Vectors.Cursor := Users.First;
            User : Samples.User.Model.User_Info;
         begin
            while Samples.User.Model.User_Info_Vectors.Has_Element (Iter) loop
               User := Samples.User.Model.User_Info_Vectors.Element (Iter);
               Ada.Text_IO.Put (ADO.Identifier'Image (User.Id));
               Ada.Text_IO.Set_Col (10);
               Ada.Text_IO.Put (To_String (User.Name));
               Ada.Text_IO.Set_Col (60);
               Ada.Text_IO.Put_Line (To_String (User.Email));
               Samples.User.Model.User_Info_Vectors.Next (Iter);
            end loop;
         end;
      end if;
   end;
end List_User;
