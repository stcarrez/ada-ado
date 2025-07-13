-----------------------------------------------------------------------
--  List_User -- Example of SQL queries
--  Copyright (C) 2010 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Exceptions;

with ADO;
with ADO.Configs;
with ADO.Sessions;
with ADO.Connections;
with ADO.Statements;
with ADO.Sessions.Factory;

with Util.Properties;

with DB_Initialize;
procedure Sql_List_User is
   Props      : Util.Properties.Manager;
   Factory    : ADO.Sessions.Factory.Session_Factory;
begin
   --  Initialize the database drivers.
   DB_Initialize (Props);

   --  Initialize the session factory to connect to the
   --  database defined by 'ado.database' property.
   Factory.Create (ADO.Configs.Get_Config ("ado.database"));

   declare
      DB      : constant ADO.Sessions.Session := Factory.Get_Session;
      Stmt    : ADO.Statements.Query_Statement;
      Count   : Natural := 0;
      Query   : constant String := "SELECT u.id, u.name, u.email FROM user AS u"
        & " ORDER BY u.name ASC";
   begin
      Stmt := DB.Create_Statement (Query);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         Ada.Text_IO.Put (ADO.Identifier'Image (Stmt.Get_Identifier (0)));
         Ada.Text_IO.Set_Col (10);
         Ada.Text_IO.Put (Stmt.Get_String (1));
         Ada.Text_IO.Set_Col (60);
         Ada.Text_IO.Put_Line (Stmt.Get_String (2));
         Stmt.Next;
         Count := Count + 1;
      end loop;

      --  Print the list
      if Count = 0 then
         Ada.Text_IO.Put_Line ("User info list is empty.");
         Ada.Text_IO.Put_Line ("Use the 'add_user' to add users in the table.");
      end if;
   end;

exception
   when E : ADO.Connections.Database_Error | ADO.Sessions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Cannot connect to database: "
                              & Ada.Exceptions.Exception_Message (E));
end Sql_List_User;
