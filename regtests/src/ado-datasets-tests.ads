-----------------------------------------------------------------------
--  ado-datasets-tests -- Test executing queries and using datasets
--  Copyright (C) 2013, 2014, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Datasets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_List (T : in out Test);

   --  Test dataset lists with null columns.
   procedure Test_List_Nullable (T : in out Test);

   procedure Test_Count (T : in out Test);

   procedure Test_Count_Query (T : in out Test);

end ADO.Datasets.Tests;
