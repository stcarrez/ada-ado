-----------------------------------------------------------------------
--  ado-sql-tests -- SQL tests
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Test_Caller;

package body ADO.SQL.Tests is

   package Caller is new Util.Test_Caller (Test, "ADO.SQL");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.SQL.Read_File",
                       Test_Read_File'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test Read_SQL file and spliting a file in SQL statements.
   --  ------------------------------
   procedure Test_Read_File (T : in out Test) is
      procedure Process (SQL : in String);

      Count : Natural := 0;

      procedure Process (SQL : in String) is
      begin
         T.Assert_Equals (")", SQL (SQL'Last .. SQL'Last));
         if SQL (SQL'First) = 'I' then
            Util.Tests.Assert_Matches (T, "INSERT .*", SQL,
                                       "invalid SQL");
         else
            Util.Tests.Assert_Matches (T, ".*CREATE TABLE.*", SQL,
                                       "invalid SQL");
         end if;
         Count := Count + 1;
      end Process;
   begin
      ADO.SQL.Read_File ("regtests/files/read-1.sql", Process'Access);
      Util.Tests.Assert_Equals (T, 6, Count, "Invalid number of SQL statements");

      Count := 0;
      ADO.SQL.Read_File ("regtests/files/read-2.sql", Process'Access);
      Util.Tests.Assert_Equals (T, 3, Count, "Invalid number of SQL statements");
   end Test_Read_File;

end ADO.SQL.Tests;
