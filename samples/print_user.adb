-----------------------------------------------------------------------
--  Print_User -- Example to find an object from the database
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
with ADO;
with ADO.Drivers.Initializer;
with ADO.Sessions;
with ADO.SQL;
with ADO.Sessions.Factory;
with Samples.User.Model;
with Util.Log.Loggers;
with Ada.Text_IO;

with Ada.Command_Line;
procedure Print_User is

   use ADO;
   use Ada;
   use Samples.User.Model;

   Factory : ADO.Sessions.Factory.Session_Factory;
begin
   Util.Log.Loggers.Initialize ("samples.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("samples.properties");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage: print_user user-name ...");
      Ada.Text_IO.Put_Line ("Example: print_user joe");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   --  Create and configure the connection pool
   Factory.Create (ADO.Drivers.Get_Config ("ado.database"));

   declare
      Session : ADO.Sessions.Session := Factory.Get_Session;
      User    : User_Ref;
      Found   : Boolean;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            User_Name : constant String := Ada.Command_Line.Argument (I);
            Query     : ADO.SQL.Query;
         begin
            Ada.Text_IO.Put_Line ("Searching '" & User_Name & "'...");
            Query.Bind_Param (1, User_Name);
            Query.Set_Filter ("name = ?");
            User.Find (Session => Session, Query => Query, Found => Found);
            if Found then
               Ada.Text_IO.Put_Line ("  Id     :  " & Identifier'Image (User.Get_Id));
               Ada.Text_IO.Put_Line ("  User   :  " & User.Get_Name);
               Ada.Text_IO.Put_Line ("  Email  :  " & User.Get_Email);
            else
               Ada.Text_IO.Put_Line ("  User '" & User_Name & "' does not exist");
            end if;
         end;
      end loop;
   end;
end Print_User;
