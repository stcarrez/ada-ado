-----------------------------------------------------------------------
--  ado-statements-tests -- Test statements package
--  Copyright (C) 2015, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Statements.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test creation of several rows in test_table with different column type.
   procedure Test_Save (T : in out Test);

   --  Test queries using the $entity_type[] cache group.
   procedure Test_Entity_Types (T : in out Test);

   --  Test executing a SQL query and getting an invalid column.
   procedure Test_Invalid_Column (T : in out Test);

   --  Test executing a SQL query and getting an invalid value.
   procedure Test_Invalid_Type (T : in out Test);

   --  Test executing a SQL query with an invalid SQL.
   procedure Test_Invalid_Statement (T : in out Test);

end ADO.Statements.Tests;
