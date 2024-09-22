-----------------------------------------------------------------------
--  ado-sql-tests -- SQL tests
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
package ADO.SQL.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test Read_SQL file and spliting a file in SQL statements.
   procedure Test_Read_File (T : in out Test);

end ADO.SQL.Tests;
