-----------------------------------------------------------------------
--  ado-queries-tests -- Test loading of database queries
--  Copyright (C) 2011, 2012, 2015, 2017, 2018, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Queries.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Load_Queries (T : in out Test);

   --  Test re-loading queries.
   procedure Test_Reload_Queries (T : in out Test);

   --  Test the Initialize operation called several times
   procedure Test_Initialize (T : in out Test);

   --  Test the Set_Query operation.
   procedure Test_Set_Query (T : in out Test);

   --  Test the Set_Limit operation.
   procedure Test_Set_Limit (T : in out Test);

   --  Test the Find_Query operation.
   procedure Test_Find_Query (T : in out Test);

   --  Test the missing query.
   procedure Test_Missing_Query (T : in out Test);

   --  Test the static query loader.
   procedure Test_Query_Loader (T : in out Test);

end ADO.Queries.Tests;
