-----------------------------------------------------------------------
--  ado-parameters-tests -- Test query parameters and SQL expansion
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Parameters.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test expand SQL with parameters.
   procedure Test_Expand_Sql (T : in out Test);

   --  Test expand with invalid parameters.
   procedure Test_Expand_Error (T : in out Test);

   --  Test expand performance.
   procedure Test_Expand_Perf (T : in out Test);

   --  Test expand with cache expander.
   procedure Test_Expand_With_Expander (T : in out Test);

end ADO.Parameters.Tests;
