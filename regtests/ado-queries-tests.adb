-----------------------------------------------------------------------
--  ado-queries-tests -- Test loading of database queries
--  Copyright (C) 2011, 2012, 2013, 2014 Stephane Carrez
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
with Util.Properties;

with ADO.Drivers.Connections;
with ADO.Queries.Loaders;
package body ADO.Queries.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "ADO.Queries");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Queries.Read_Query",
                       Test_Load_Queries'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Initialize",
                       Test_Initialize'Access);
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
      use ADO.Drivers.Connections;

      Mysql_Driver  : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("sqlite");
      Props         : constant Util.Properties.Manager := Util.Tests.Get_Properties;
   begin
      --  Configure the XML query loader.
      ADO.Queries.Loaders.Initialize (Props.Get ("ado.queries.paths", ".;db"),
                                      Props.Get ("ado.queries.load", "false") = "true");

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

      if Mysql_Driver /= null then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Mysql_Driver.Get_Driver_Index);
         begin
            Assert_Equals (T, "select 1", SQL, "Invalid query for 'index' (MySQL driver)");
         end;
      end if;
      if Sqlite_Driver /= null then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Sqlite_Driver.Get_Driver_Index);
         begin
            Assert_Equals (T, "select 0", SQL, "Invalid query for 'index' (SQLite driver)");
         end;
      end if;
   end Test_Load_Queries;

   --  ------------------------------
   --  Test the Initialize operation called several times
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      use ADO.Drivers.Connections;

      Props : constant Util.Properties.Manager := Util.Tests.Get_Properties;
      Info  : Query_Info_Ref.Ref;
   begin
      --  Configure and load the XML queries.
      ADO.Queries.Loaders.Initialize (Props.Get ("ado.queries.paths", ".;db"), True);
      T.Assert (not Simple_Query.Query.Query.Get.Is_Null, "The simple query was not loaded");
      T.Assert (not Index_Query.Query.Query.Get.Is_Null, "The index query was not loaded");
      Info := Simple_Query.Query.Query.Get;

      --  Re-configure but do not reload.
      ADO.Queries.Loaders.Initialize (Props.Get ("ado.queries.paths", ".;db"), False);
      T.Assert (Info.Value = Simple_Query.Query.Query.Get.Value,
                "The simple query instance was not changed");

      --  Configure again and reload.  The query info must have changed.
      ADO.Queries.Loaders.Initialize (Props.Get ("ado.queries.paths", ".;db"), True);
      T.Assert (Info.Value /= Simple_Query.Query.Query.Get.Value,
                "The simple query instance was not changed");

      --  Due to the reference held by 'Info', it refers to the data loaded first.
      T.Assert (Length (Info.Value.Main_Query (0).SQL) > 0, "The old query is not valid");
   end Test_Initialize;

end ADO.Queries.Tests;
