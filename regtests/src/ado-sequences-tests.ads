-----------------------------------------------------------------------
--  ado-sequences-tests -- Test sequences factories
--  Copyright (C) 2012-2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Sequences.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Create_Factory (T : in out Test);

   procedure Test_Snowflake_Factory (T : in out Test);

end ADO.Sequences.Tests;
