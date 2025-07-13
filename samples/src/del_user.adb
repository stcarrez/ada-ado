-----------------------------------------------------------------------
--  Del_User -- Example to find an object from the database
--  Copyright (C) 2010 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;

with ADO;
with ADO.Configs;
with ADO.Sessions;
with ADO.Connections;
with ADO.SQL;
with ADO.Sessions.Factory;

with Util.Properties;

with DB_Initialize;
with Samples.User.Model;
procedure Del_User is
   use Samples.User.Model;

   Props   : Util.Properties.Manager;
   Factory : ADO.Sessions.Factory.Session_Factory;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage: del_user user-name ...");
      Ada.Text_IO.Put_Line ("Example: del_user joe");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   --  Initialize the database drivers.
   DB_Initialize (Props);

   --  Create and configure the connection pool
   Factory.Create (ADO.Configs.Get_Config ("ado.database"));

   declare
      Session : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
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
               User.Delete (Session);
               Ada.Text_IO.Put_Line ("  User '" & User_Name & "' deleted");
            else
               Ada.Text_IO.Put_Line ("  User '" & User_Name & "' does not exist");
            end if;
         end;
      end loop;
   end;

exception
   when E : ADO.Connections.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
end Del_User;
