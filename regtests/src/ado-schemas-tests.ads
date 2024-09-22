-----------------------------------------------------------------------
--  ado-schemas-tests -- Test loading of database schema
--  Copyright (C) 2009, 2010, 2011, 2015, 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Schemas.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading the entity cache and the Find_Entity_Type operation
   procedure Test_Find_Entity_Type (T : in out Test);

   --  Test calling Find_Entity_Type with an invalid table.
   procedure Test_Find_Entity_Type_Error (T : in out Test);

   --  Test the Load_Schema operation and check the result schema.
   procedure Test_Load_Schema (T : in out Test);

   --  Test the Table_Cursor operations and check the result schema.
   procedure Test_Table_Iterator (T : in out Test);

   --  Test the Has_Table operation
   procedure Test_Has_Table (T : in out Test);

   --  Test the Table_Cursor operations on an empty schema.
   procedure Test_Empty_Schema (T : in out Test);

   --  Test the creation of database.
   procedure Test_Create_Schema (T : in out Test);

   --  Test the sort migration.
   procedure Test_Sort_Migration (T : in out Test);

   --  Test the scan of migration.
   procedure Test_Scan_Migration (T : in out Test);

end ADO.Schemas.Tests;
