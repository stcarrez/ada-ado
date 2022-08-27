-----------------------------------------------------------------------
--  ado-sql-tests -- SQL tests
--  Copyright (C) 2022 Stephane Carrez
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
