-----------------------------------------------------------------------
--  ado-drivers-tests -- Unit tests for database drivers
--  Copyright (C) 2014, 2015, 2016, 2018 Stephane Carrez
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
with Ada.Exceptions;

with Util.Test_Caller;

with Regtests;

with ADO.Configs;
with ADO.Statements;
with ADO.Sessions;
with ADO.Drivers.Connections;
package body ADO.Drivers.Tests is

   use ADO.Configs;
   use ADO.Drivers.Connections;

   package Caller is new Util.Test_Caller (Test, "ADO.Drivers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Drivers.Initialize",
                       Test_Initialize'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Config",
                       Test_Get_Config'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver",
                       Test_Get_Driver'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver_Index",
                       Test_Get_Driver_Index'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver",
                       Test_Load_Invalid_Driver'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Connection",
                       Test_Set_Connection'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Connection (Errors)",
                       Test_Set_Connection_Error'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Server",
                       Test_Set_Connection_Server'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Port",
                       Test_Set_Connection_Port'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Database",
                       Test_Set_Connection_Database'Access);
      Caller.Add_Test (Suite, "Test ADO.Databases (Errors)",
                       Test_Empty_Connection'Access);
      Caller.Add_Test (Suite, "Test ADO.Databases (DB Closed errors)",
                       Test_Closed_Connection'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Initialize operation.
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      pragma Unreferenced (T);
   begin
      ADO.Drivers.Initialize ("test-missing-config.properties");
   end Test_Initialize;

   --  ------------------------------
   --  Test the Get_Config operation.
   --  ------------------------------
   procedure Test_Get_Config (T : in out Test) is
   begin
      T.Assert (ADO.Drivers.Get_Config ("test.database")'Length > 0,
                "The Get_Config operation returned no value for the test database");
   end Test_Get_Config;

   --  ------------------------------
   --  Test the Get_Driver operation.
   --  ------------------------------
   procedure Test_Get_Driver (T : in out Test) is
      Mysql_Driver  : constant Driver_Access := Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := Drivers.Connections.Get_Driver ("sqlite");
      Postgres_Driver : constant Driver_Access := Drivers.Connections.Get_Driver ("postgresql");
   begin
      T.Assert (Mysql_Driver /= null or Sqlite_Driver /= null or Postgres_Driver /= null,
                "No database driver was found!");
   end Test_Get_Driver;

   --  ------------------------------
   --  Test loading some invalid database driver.
   --  ------------------------------
   procedure Test_Load_Invalid_Driver (T : in out Test) is
      Invalid_Driver  : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("invalid");
   begin
      T.Assert (Invalid_Driver = null,
                "The Get_Driver operation must return null for invalid drivers");
   end Test_Load_Invalid_Driver;

   --  ------------------------------
   --  Test the Get_Driver_Index operation.
   --  ------------------------------
   procedure Test_Get_Driver_Index (T : in out Test) is
      Mysql_Driver  : constant Driver_Access := Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := Drivers.Connections.Get_Driver ("sqlite");
      Postgres_Driver : constant Driver_Access := Drivers.Connections.Get_Driver ("postgresql");
   begin
      if Mysql_Driver /= null then
         T.Assert (Mysql_Driver.Get_Driver_Index > 0, "The driver index must be positive");
      end if;
      if Sqlite_Driver /= null then
         T.Assert (Sqlite_Driver.Get_Driver_Index > 0, "The driver index must be positive");
      end if;
      if Postgres_Driver /= null then
         T.Assert (Postgres_Driver.Get_Driver_Index > 0, "The driver index must be positive");
      end if;
      if Mysql_Driver /= null and Sqlite_Driver /= null then
         T.Assert (Mysql_Driver.Get_Driver_Index /= Sqlite_Driver.Get_Driver_Index,
                   "Two drivers must have different driver indexes");
      end if;
      if Mysql_Driver /= null and Postgres_Driver /= null then
         T.Assert (Mysql_Driver.Get_Driver_Index /= Postgres_Driver.Get_Driver_Index,
                   "Two drivers must have different driver indexes");
      end if;
      if Sqlite_Driver /= null and Postgres_Driver /= null then
         T.Assert (Sqlite_Driver.Get_Driver_Index /= Postgres_Driver.Get_Driver_Index,
                   "Two drivers must have different driver indexes");
      end if;
   end Test_Get_Driver_Index;

   --  ------------------------------
   --  Test the Set_Connection procedure.
   --  ------------------------------
   procedure Test_Set_Connection (T : in out Test) is

      procedure Check (URI      : in String;
                       Server   : in String;
                       Port     : in Integer;
                       Database : in String);

      procedure Check (URI      : in String;
                       Server   : in String;
                       Port     : in Integer;
                       Database : in String) is
         Controller : ADO.Drivers.Connections.Configuration;
      begin
         Controller.Set_Connection (URI);
         Util.Tests.Assert_Equals (T, Server, Controller.Get_Server, "Invalid server for " & URI);
         Util.Tests.Assert_Equals (T, Port, Controller.Get_Port, "Invalid port for " & URI);
         Util.Tests.Assert_Equals (T, Database, Controller.Get_Database, "Invalid db for " & URI);

         Controller.Set_Property ("password", "test");
         Util.Tests.Assert_Equals (T, "test", Controller.Get_Property ("password"),
                                   "Invalid 'password' property for " & URI);

      exception
         when E : Connection_Error =>
            Util.Tests.Assert_Matches (T, "Driver.*not found.*",
                                       Ada.Exceptions.Exception_Message (E),
                                       "Invalid exception raised for " & URI);
      end Check;

   begin
      Check ("mysql://test:3306/db", "test", 3306, "db");
      Check ("mysql://test2:3307/db2?user=admin&password=admin", "test2", 3307, "db2");
      Check ("sqlite:///test.db", "", 0, "test.db");
      Check ("sqlite:///test2.db?user=root&encoding=UTF-8", "", 0, "test2.db");
   end Test_Set_Connection;

   --  ------------------------------
   --  Test the Set_Connection procedure with several error cases.
   --  ------------------------------
   procedure Test_Set_Connection_Error (T : in out Test) is

      procedure Check_Invalid_Connection (URI : in String);

      Controller : ADO.Drivers.Connections.Configuration;

      procedure Check_Invalid_Connection (URI : in String) is
      begin
         Controller.Set_Connection (URI);
         T.Fail ("No Connection_Error exception raised for " & URI);

      exception
         when Connection_Error =>
            null;

      end Check_Invalid_Connection;

   begin
      Check_Invalid_Connection ("");
      Check_Invalid_Connection ("http://");
      Check_Invalid_Connection ("mysql://");
      Check_Invalid_Connection ("sqlite://");
      Check_Invalid_Connection ("mysql://:toto/");
      Check_Invalid_Connection ("sqlite://:toto/");
   end Test_Set_Connection_Error;

   --  ------------------------------
   --  Test the Set_Server operation.
   --  ------------------------------
   procedure Test_Set_Connection_Server (T : in out Test) is
      Controller : ADO.Drivers.Connections.Configuration;
   begin
      Controller.Set_Connection ("mysql://localhost:3306/db");
      Controller.Set_Server ("server-name");
      Util.Tests.Assert_Equals (T, "server-name", Controller.Get_Server,
                                "Configuration Set_Server returned invalid value");

   exception
      when E : ADO.Configs.Connection_Error =>
         Util.Tests.Assert_Equals (T, "Driver 'mysql' not found",
                                   Ada.Exceptions.Exception_Message (E),
                                   "Invalid exception message");
   end Test_Set_Connection_Server;

   --  ------------------------------
   --  Test the Set_Port operation.
   --  ------------------------------
   procedure Test_Set_Connection_Port (T : in out Test) is
      Controller : ADO.Drivers.Connections.Configuration;
   begin
      Controller.Set_Connection ("mysql://localhost:3306/db");
      Controller.Set_Port (1234);
      Util.Tests.Assert_Equals (T, 1234, Controller.Get_Port,
                                "Configuration Set_Port returned invalid value");

   exception
      when E : ADO.Configs.Connection_Error =>
         Util.Tests.Assert_Equals (T, "Driver 'mysql' not found",
                                   Ada.Exceptions.Exception_Message (E),
                                   "Invalid exception message");
   end Test_Set_Connection_Port;

   --  ------------------------------
   --  Test the Set_Database operation.
   --  ------------------------------
   procedure Test_Set_Connection_Database (T : in out Test) is
      Controller : ADO.Drivers.Connections.Configuration;
   begin
      Controller.Set_Connection ("mysql://localhost:3306/db");
      Controller.Set_Database ("test-database");
      Util.Tests.Assert_Equals (T, "test-database", Controller.Get_Database,
                                "Configuration Set_Database returned invalid value");

   exception
      when E : ADO.Configs.Connection_Error =>
         Util.Tests.Assert_Equals (T, "Driver 'mysql' not found",
                                   Ada.Exceptions.Exception_Message (E),
                                   "Invalid exception message");
   end Test_Set_Connection_Database;

   --  ------------------------------
   --  Test the connection operations on an empty connection.
   --  ------------------------------
   procedure Test_Empty_Connection (T : in out Test) is
      use type ADO.Sessions.Connection_Status;

      C    : ADO.Sessions.Session;
      Stmt : ADO.Statements.Query_Statement;

      pragma Unreferenced (Stmt);
   begin
      T.Assert (C.Get_Status = ADO.Sessions.CLOSED,
                "The database connection must be closed for an empty connection");

      --  Create_Statement must raise Session_Error.
      begin
         Stmt := C.Create_Statement ("");
         T.Fail ("No exception raised for Create_Statement");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;

      --  Create_Statement must raise Session_Error.
      begin
         Stmt := C.Create_Statement ("select");
         T.Fail ("No exception raised for Create_Statement");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;

   end Test_Empty_Connection;

   --  ------------------------------
   --  Test the connection operations on a closed connection.
   --  ------------------------------
   procedure Test_Closed_Connection (T : in out Test) is
   begin
      declare
         DB    : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
         DB2   : constant ADO.Sessions.Master_Session := DB;
         Stmt  : ADO.Statements.Query_Statement;
      begin
         DB.Close;
         Stmt := DB2.Create_Statement ("SELECT name FROM test_table");
         Stmt.Execute;
         Util.Tests.Fail (T, "No Session_Error exception was raised");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;
      declare
         DB    : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
         DB2   : ADO.Sessions.Master_Session := DB;
      begin
         DB.Close;
         DB2.Begin_Transaction;
         Util.Tests.Fail (T, "No Session_Error exception was raised");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;
      declare
         DB    : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
         DB2   : ADO.Sessions.Master_Session := DB;
      begin
         DB.Close;
         DB2.Commit;
         Util.Tests.Fail (T, "No Session_Error exception was raised");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;
      declare
         DB    : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
         DB2   : ADO.Sessions.Master_Session := DB;
      begin
         DB.Close;
         DB2.Rollback;
         Util.Tests.Fail (T, "No Session_Error exception was raised");

      exception
         when ADO.Sessions.Session_Error =>
            null;
      end;
   end Test_Closed_Connection;

end ADO.Drivers.Tests;
