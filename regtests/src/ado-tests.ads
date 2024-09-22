-----------------------------------------------------------------------
--  ado-tests -- Various tests on database access
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      I1 : Integer;
      I2 : Integer;
   end record;

   overriding
   procedure Set_Up (T : in out Test);

   procedure Test_Load (T : in out Test);

   procedure Test_Create_Load (T : in out Test);

   procedure Test_Not_Open (T : in out Test);

   procedure Test_Allocate (T : in out Test);

   procedure Test_Create_Save (T : in out Test);

   procedure Test_Perf_Create_Save (T : in out Test);
   procedure Test_Delete_All (T : in out Test);

   --  Test string insert.
   procedure Test_String (T : in out Test);

   --  Test blob insert.
   procedure Test_Blob (T : in out Test);

   --  Test the To_Object and To_Identifier operations.
   procedure Test_Identifier_To_Object (T : in out Test);

   --  Test database reload.
   procedure Test_Reload (T : in out Test);

   --  Test some concurrency aspects.
   procedure Test_Concurrency (T : in out Test);

end ADO.Tests;
