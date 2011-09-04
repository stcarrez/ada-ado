-----------------------------------------------------------------------
--  ado-queries-tests -- Test loading of database queries
--  Copyright (C) 2011 Stephane Carrez
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
with ADO.Queries.Loaders;
package body ADO.Queries.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Queries.Read_Query",
                       Test_Load_Queries'Access);
   end Add_Tests;

   package Simple_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/simple-query.xml",
                                   Sha1 => "");

   package Multi_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/multi-query.xml",
                                   Sha1 => "");

   package Simple_Query is
     new ADO.Queries.Loaders.Query (Name => "simple-query",
                                    File => Simple_Query_File.File'Access);

   package Simple_Query_2 is
     new ADO.Queries.Loaders.Query (Name => "simple-query",
                                    File => Multi_Query_File.File'Access);

   package Index_Query is
     new ADO.Queries.Loaders.Query (Name => "index",
                                    File => Multi_Query_File.File'Access);

   package Value_Query is
     new ADO.Queries.Loaders.Query (Name => "value",
                                    File => Multi_Query_File.File'Access);

   pragma Warnings (Off, Simple_Query_2);
   pragma Warnings (Off, Value_Query);

   procedure Test_Load_Queries (T : in out Test) is
   begin
      declare
         SQL : constant String := ADO.Queries.Get_SQL (Simple_Query.Query'Access, 0);
      begin
         Assert_Equals (T, "select count(*) from user", SQL, "Invalid query for 'simple-query'");
      end;

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access, 0);
      begin
         Assert_Equals (T, "select 0", SQL, "Invalid query for 'index'");
      end;

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access, 1);
      begin
         Assert_Equals (T, "select 1", SQL, "Invalid query for 'index'");
      end;
   end Test_Load_Queries;

end ADO.Queries.Tests;
