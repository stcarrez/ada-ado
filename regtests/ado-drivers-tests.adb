-----------------------------------------------------------------------
--  ado-drivers-tests -- Unit tests for database drivers
--  Copyright (C) 2014, 2015 Stephane Carrez
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
with Util.Test_Caller;

with ADO.Drivers.Connections;
package body ADO.Drivers.Tests is

   use ADO.Drivers.Connections;

   package Caller is new Util.Test_Caller (Test, "ADO.Drivers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Config",
                       Test_Get_Config'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver",
                       Test_Get_Driver'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver_Index",
                       Test_Get_Driver_Index'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Get_Driver",
                       Test_Load_Invalid_Driver'Access);
      Caller.Add_Test (Suite, "Test ADO.Drivers.Connections.Set_Connection (Errors)",
                       Test_Set_Connection_Error'Access);
   end Add_Tests;

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
      Mysql_Driver  : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("sqlite");
   begin
      T.Assert (Mysql_Driver /= null or Sqlite_Driver /= null,
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
      Mysql_Driver  : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("sqlite");
   begin
      if Mysql_Driver /= null then
         T.Assert (Mysql_Driver.Get_Driver_Index > 0, "The driver index must be positive");
      end if;
      if Sqlite_Driver /= null then
         T.Assert (Sqlite_Driver.Get_Driver_Index > 0, "The driver index must be positive");
      end if;
      if Mysql_Driver /= null and Sqlite_Driver /= null then
         T.Assert (Mysql_Driver.Get_Driver_Index /= Sqlite_Driver.Get_Driver_Index,
                   "Two drivers must have different driver indexes");
      end if;
   end Test_Get_Driver_Index;

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

end ADO.Drivers.Tests;
