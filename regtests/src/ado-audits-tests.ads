-----------------------------------------------------------------------
--  ado-audits-tests -- Audit tests
--  Copyright (C) 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.Audits.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   overriding
   procedure Set_Up (T : in out Test);

   --  Test populating Audit_Fields
   procedure Test_Audit_Field (T : in out Test);

end ADO.Audits.Tests;
