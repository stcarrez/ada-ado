-----------------------------------------------------------------------
--  select_user - Show usage of query statements
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Drivers;
with ADO.Configs;
with ADO.Sessions;
with ADO.Connections;
with ADO.Sessions.Factory;
with ADO.Statements;

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Util.Log.Loggers;

procedure Select_User is
   Factory : ADO.Sessions.Factory.Session_Factory;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Usage: select_user user-name ...");
      Ada.Text_IO.Put_Line ("Example: select_user joe");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   Util.Log.Loggers.Initialize ("samples.properties");

   --  Initialize the database drivers.
   ADO.Drivers.Initialize ("samples.properties");

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Factory.Create (ADO.Configs.Get_Config ("ado.database"));

   declare
      DB     : constant ADO.Sessions.Session := Factory.Get_Session;
      Stmt   : ADO.Statements.Query_Statement
        := DB.Create_Statement ("SELECT "
                                & "id, name, email, date, description, status "
                                & "FROM user WHERE name = :name");
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         Stmt.Bind_Param (Name  => "name",
                          Value => Ada.Command_Line.Argument (I));
         Stmt.Execute;

         while Stmt.Has_Elements loop
            Ada.Text_IO.Put_Line ("  Id     :  " & Stmt.Get_String (0));
            Ada.Text_IO.Put_Line ("  User   :  " & Stmt.Get_String (1));
            Ada.Text_IO.Put_Line ("  Email  :  " & Stmt.Get_String (2));
            Stmt.Next;
         end loop;
      end loop;
   end;

exception
   when E : ADO.Connections.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
end Select_User;
