-----------------------------------------------------------------------
--  ado-drivers-tests -- Unit tests for database drivers
--  Copyright (C) 2014, 2015, 2016, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Drivers.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Initialize operation.
   procedure Test_Initialize (T : in out Test);

   --  Test the Get_Config operation.
   procedure Test_Get_Config (T : in out Test);

   --  Test the Get_Driver operation.
   procedure Test_Get_Driver (T : in out Test);

   --  Test loading some invalid database driver.
   procedure Test_Load_Invalid_Driver (T : in out Test);

   --  Test the Get_Driver_Index operation.
   procedure Test_Get_Driver_Index (T : in out Test);

   --  Test the Set_Connection procedure.
   procedure Test_Set_Connection (T : in out Test);

   --  Test the Set_Connection procedure with several error cases.
   procedure Test_Set_Connection_Error (T : in out Test);

   --  Test the Set_Server operation.
   procedure Test_Set_Connection_Server (T : in out Test);

   --  Test the Set_Port operation.
   procedure Test_Set_Connection_Port (T : in out Test);

   --  Test the Set_Database operation.
   procedure Test_Set_Connection_Database (T : in out Test);

   --  Test the connection operations on an empty connection.
   procedure Test_Empty_Connection (T : in out Test);

   --  Test the connection operations on a closed connection.
   procedure Test_Closed_Connection (T : in out Test);

   --  Test opening an invalid connection and make sure we get some error.
   procedure Test_Invalid_Connection (T : in out Test);

end ADO.Drivers.Tests;
