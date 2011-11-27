-----------------------------------------------------------------------
--  ado-testsuite -- Testsuite for ADO
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with ADO.Tests;
with ADO.Schemas.Tests;
with ADO.Objects.Tests;
with ADO.Queries.Tests;
with ADO.Parameters.Tests;
package body ADO.Testsuite is

   use ADO.Tests;

   procedure Drivers (Suite : in Util.Tests.Access_Test_Suite);

   procedure Drivers (Suite : in Util.Tests.Access_Test_Suite) is separate;

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      ADO.Parameters.Tests.Add_Tests (Ret);
      ADO.Objects.Tests.Add_Tests (Ret);
      ADO.Tests.Add_Tests (Ret);
      ADO.Schemas.Tests.Add_Tests (Ret);
      Drivers (Ret);
      ADO.Queries.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end ADO.Testsuite;
