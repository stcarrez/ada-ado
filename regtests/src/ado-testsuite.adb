-----------------------------------------------------------------------
--  ado-testsuite -- Testsuite for ADO
--  Copyright (C) 2009 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Tests;
with ADO.Drivers.Tests;
with ADO.Sequences.Tests;
with ADO.Schemas.Tests;
with ADO.Objects.Tests;
with ADO.Queries.Tests;
with ADO.Parameters.Tests;
with ADO.Datasets.Tests;
with ADO.Statements.Tests;
with ADO.Audits.Tests;
with ADO.SQL.Tests;
package body ADO.Testsuite is

   --  procedure Drivers (Suite : in Util.Tests.Access_Test_Suite);

   --  procedure Drivers (Suite : in Util.Tests.Access_Test_Suite) is separate;

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      ADO.Drivers.Tests.Add_Tests (Ret);
      ADO.Parameters.Tests.Add_Tests (Ret);
      ADO.Sequences.Tests.Add_Tests (Ret);
      ADO.Objects.Tests.Add_Tests (Ret);
      ADO.Statements.Tests.Add_Tests (Ret);
      ADO.Tests.Add_Tests (Ret);
      ADO.Schemas.Tests.Add_Tests (Ret);
      --  Drivers (Ret);
      ADO.Queries.Tests.Add_Tests (Ret);
      ADO.Datasets.Tests.Add_Tests (Ret);
      ADO.Audits.Tests.Add_Tests (Ret);
      ADO.SQL.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end ADO.Testsuite;
