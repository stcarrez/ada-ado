-----------------------------------------------------------------------
--  Add_User -- Example to add an object in the database
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
with ADO.Sessions;
with ADO.Sessions.Factory;

with Util.Strings;
with Util.Log.Loggers;

with Ada.Text_IO;
with Ada.Command_Line;
procedure Add_User is

   use Samples.User.Model;

   function Get_Name (Email : in String) return String;

   Factory    : ADO.Sessions.Factory.Session_Factory;

   function Get_Name (Email : in String) return String is
      Pos : constant Natural := Util.Strings.Index (Email, '@');
   begin
      if Pos > 0 then
         return Email (Email'First .. Pos - 1);
      else
         return Email;
      end if;
   end Get_Name;

begin
   Util.Log.Loggers.Initialize ("samples.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("samples.properties");

   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage: add_user user-email ...");
      Ada.Text_IO.Put_Line ("Example: add_user joe.potter@gmail.com");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Factory.Create (ADO.Drivers.Get_Config ("ado.database"));

   declare
      DB    : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
   begin
      DB.Begin_Transaction;
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Email : constant String := Ada.Command_Line.Argument (I);
            Name  : constant String := Get_Name (Email);
            User  : User_Ref;
         begin
            User.Set_Name (Name);
            User.Set_Email (Email);
            User.Set_Description ("My friend " & Name);
            User.Set_Status (0);
            User.Save (DB);

            Ada.Text_IO.Put_Line ("User " & Name & " has id "
                                  & ADO.Identifier'Image (User.Get_Id));
         end;
      end loop;
      DB.Commit;
   end;
end Add_User;
