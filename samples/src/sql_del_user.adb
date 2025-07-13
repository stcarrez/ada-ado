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
with ADO.Statements;
with ADO.Sessions.Factory;

with Util.Properties;

with DB_Initialize;
procedure Sql_Del_User is
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
      Session : constant ADO.Sessions.Master_Session := Factory.Get_Master_Session;
      Query   : constant String := "DELETE FROM user WHERE name=?";
      Stmt    : ADO.Statements.Delete_Statement;
   begin
      Stmt := Session.Create_Statement (Query);
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            User_Name : constant String := Ada.Command_Line.Argument (I);
            Result    : Integer;
         begin
            Ada.Text_IO.Put_Line ("Searching '" & User_Name & "'...");
            Stmt.Bind_Param (1, User_Name);
            Stmt.Execute (Result);
            if Result > 0 then
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
end Sql_Del_User;
