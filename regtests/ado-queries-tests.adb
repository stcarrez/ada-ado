-----------------------------------------------------------------------
--  ado-queries-tests -- Test loading of database queries
--  Copyright (C) 2011 - 2021 Stephane Carrez
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

with ADO.Connections;
with ADO.Queries.Loaders;
package body ADO.Queries.Tests is

   use Util.Tests;

   function Loader (Name : in String) return access constant String;

   package Caller is new Util.Test_Caller (Test, "ADO.Queries");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Queries.Read_Query",
                       Test_Load_Queries'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Find_Query",
                       Test_Find_Query'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Initialize",
                       Test_Initialize'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Read_Query (reload)",
                       Test_Reload_Queries'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Set_Query",
                       Test_Set_Query'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Set_Limit",
                       Test_Set_Limit'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Get_SQL (raise Query_Error)",
                       Test_Missing_Query'Access);
      Caller.Add_Test (Suite, "Test ADO.Queries.Set_Query_Loader",
                       Test_Query_Loader'Access);
   end Add_Tests;

   package Simple_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/simple-query.xml",
                                   Sha1 => "");

   package Multi_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/multi-query.xml",
                                   Sha1 => "");

   package Missing_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/missing-query.xml",
                                   Sha1 => "");

   package Internal_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/internal-query.xml",
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

   package Internal_Query is
     new ADO.Queries.Loaders.Query (Name => "internal-query",
                                    File => Internal_Query_File.File'Access);

   package Missing_Query_SQLite is
     new ADO.Queries.Loaders.Query (Name => "missing-query-sqlite",
                                    File => Missing_Query_File.File'Access);

   package Missing_Query_MySQL is
     new ADO.Queries.Loaders.Query (Name => "missing-query-mysql",
                                    File => Missing_Query_File.File'Access);

   package Missing_Query_Postgresql is
     new ADO.Queries.Loaders.Query (Name => "missing-query-postgresql",
                                    File => Missing_Query_File.File'Access);

   package Missing_Query is
     new ADO.Queries.Loaders.Query (Name => "missing-query",
                                    File => Missing_Query_File.File'Access);

   pragma Warnings (Off, Simple_Query_2);
   pragma Warnings (Off, Value_Query);
   pragma Warnings (Off, Missing_Query);
   pragma Warnings (Off, Missing_Query_SQLite);
   pragma Warnings (Off, Missing_Query_MySQL);
   pragma Warnings (Off, Missing_Query_Postgresql);
   pragma Warnings (Off, Internal_Query);

   procedure Test_Load_Queries (T : in out Test) is
      use ADO.Connections;
      use type ADO.Configs.Driver_Index;

      Mysql_Driver      : constant Driver_Access := ADO.Connections.Get_Driver ("mysql");
      Sqlite_Driver     : constant Driver_Access := ADO.Connections.Get_Driver ("sqlite");
      Psql_Driver       : constant Driver_Access := ADO.Connections.Get_Driver ("postgresql");
      Config            : ADO.Connections.Configuration;
      Manager           : Query_Manager;
      Config_URL        : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                       "sqlite:///regtests.db");
   begin
      --  Configure the XML query loader.
      Config.Set_Connection (Config_URL);
      ADO.Queries.Loaders.Initialize (Manager, Config);

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Simple_Query.Query'Access, Manager, False);
      begin
         Assert_Equals (T, "select count(*) from user", SQL, "Invalid query for 'simple-query'");
      end;

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access, Manager, False);
      begin
         if Mysql_Driver /= null and then Manager.Driver = Mysql_Driver.Get_Driver_Index then
            Assert_Equals (T, "select 1", SQL, "Invalid query for 'index'");
         elsif Psql_Driver /= null and then Manager.Driver = Psql_Driver.Get_Driver_Index then
            Assert_Equals (T, "select 3", SQL, "Invalid query for 'index'");
         else
            Assert_Equals (T, "select 0", SQL, "Invalid query for 'index'");
         end if;
      end;

      if Mysql_Driver /= null and then Manager.Driver = Mysql_Driver.Get_Driver_Index then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Manager,
                                                          False);
         begin
            Assert_Equals (T, "select 1", SQL, "Invalid query for 'index' (MySQL driver)");
         end;
      end if;
      if Sqlite_Driver /= null and then Manager.Driver = Sqlite_Driver.Get_Driver_Index then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Manager, False);
         begin
            Assert_Equals (T, "select 0", SQL, "Invalid query for 'index' (SQLite driver)");
         end;
      end if;
      if Psql_Driver /= null and then Manager.Driver = Psql_Driver.Get_Driver_Index
      then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Manager,
                                                          False);
         begin
            Assert_Equals (T, "select 3", SQL, "Invalid query for 'index' (PostgreSQL driver)");
         end;
      end if;
   end Test_Load_Queries;

   --  ------------------------------
   --  Test re-loading queries.
   --  ------------------------------
   procedure Test_Reload_Queries (T : in out Test) is
      Config  : ADO.Connections.Configuration;
      Manager : Query_Manager;
      Query   : ADO.Queries.Context;
      Config_URL : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                "sqlite:///regtests.db");
   begin
      --  Configure the XML query loader.
      Config.Set_Connection (Config_URL);
      ADO.Queries.Loaders.Initialize (Manager, Config);
      for I in 1 .. 10 loop
         Query.Set_Query ("simple-query");

         declare
            SQL : constant String := Query.Get_SQL (Manager);
         begin
            Assert_Equals (T, "select count(*) from user", SQL,
                           "Invalid query for 'simple-query'");
         end;
         for J in Manager.Files'Range loop
            Manager.Files (J).Next_Check := 0;
         end loop;
      end loop;
   end Test_Reload_Queries;

   --  ------------------------------
   --  Test the Initialize operation called several times
   --  ------------------------------
   procedure Test_Initialize (T : in out Test) is
      Config  : ADO.Connections.Configuration;
      Manager : Query_Manager;
      Pos     : Query_Index;
      Config_URL : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                "sqlite:///regtests.db");
   begin
      Config.Set_Connection (Config_URL);

      --  Configure and load the XML queries.
      for Pass in 1 .. 10 loop
         Config.Set_Property ("ado.queries.load", (if Pass = 1 then "false" else "true"));
         ADO.Queries.Loaders.Initialize (Manager, Config);
         T.Assert (Manager.Queries /= null, "The queries table is allocated");
         T.Assert (Manager.Files /= null, "The files table is allocated");
         Pos := 1;
         for Query of Manager.Queries.all loop
            if Pass = 1 then
               T.Assert (Query.Is_Null, "Query must not be loaded");
            elsif Missing_Query.Query.Query /= Pos
              and Internal_Query.Query.Query /= Pos
            then
               T.Assert (not Query.Is_Null, "Query must have been loaded");
            else
               T.Assert (Query.Is_Null, "Query must not be loaded (not found)");
            end if;
            Pos := Pos + 1;
         end loop;
      end loop;

   end Test_Initialize;

   --  ------------------------------
   --  Test the Set_Query operation.
   --  ------------------------------
   procedure Test_Set_Query (T : in out Test) is
      Query   : ADO.Queries.Context;
      Manager : Query_Manager;
      Config  : ADO.Connections.Configuration;
      Config_URL : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                "sqlite:///regtests.db");
   begin
      Config.Set_Connection (Config_URL);
      ADO.Queries.Loaders.Initialize (Manager, Config);
      Query.Set_Query ("simple-query");

      declare
         SQL : constant String := Query.Get_SQL (Manager);
      begin
         Assert_Equals (T, "select count(*) from user", SQL, "Invalid query for 'simple-query'");
      end;
   end Test_Set_Query;

   --  ------------------------------
   --  Test the Set_Limit operation.
   --  ------------------------------
   procedure Test_Set_Limit (T : in out Test) is
      Query : ADO.Queries.Context;
   begin
      Query.Set_Query ("index");
      Query.Set_Limit (0, 10);
      Assert_Equals (T, 0, Query.Get_First_Row_Index, "Invalid first row index");
      Assert_Equals (T, 10, Query.Get_Last_Row_Index, "Invalid last row index");
   end Test_Set_Limit;

   --  ------------------------------
   --  Test the Find_Query operation.
   --  ------------------------------
   procedure Test_Find_Query (T : in out Test) is
      Q : Query_Definition_Access;
   begin
      Q := ADO.Queries.Loaders.Find_Query ("this query does not exist");
      T.Assert (Q = null, "Find_Query should return null for unkown query");
   end Test_Find_Query;

   --  ------------------------------
   --  Test the missing query.
   --  ------------------------------
   procedure Test_Missing_Query (T : in out Test) is
      Query   : ADO.Queries.Context;
      Manager : Query_Manager;
      Config  : ADO.Connections.Configuration;
      Count   : Natural := 0;
      Config_URL : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                "sqlite:///regtests.db");
   begin
      Config.Set_Connection (Config_URL);
      ADO.Queries.Loaders.Initialize (Manager, Config);
      Query.Set_Query ("missing-query");

      begin
         Assert_Equals (T, "?", Query.Get_SQL (Manager));
         T.Fail ("No ADO.Queries.Query_Error exception was raised");

      exception
         when ADO.Queries.Query_Error =>
            null;
      end;

      begin
         Query.Set_Query ("missing-query-sqlite");
         Assert_Equals (T, "select count(*) from user", Query.Get_SQL (Manager));

      exception
         when ADO.Queries.Query_Error =>
            Count := Count + 1;
      end;

      begin
         Query.Set_Query ("missing-query-mysql");
         Assert_Equals (T, "select count(*) from user", Query.Get_SQL (Manager));

      exception
         when ADO.Queries.Query_Error =>
            Count := Count + 1;
      end;

      begin
         Query.Set_Query ("missing-query-postgresql");
         Assert_Equals (T, "select count(*) from user", Query.Get_SQL (Manager));

      exception
         when ADO.Queries.Query_Error =>
            Count := Count + 1;
      end;
      T.Assert (Count > 0, "No Query_Error exception was raised");
   end Test_Missing_Query;

   Q1 : aliased constant String := "<query-mapping><query name='internal-query'>"
     & "<sql>SELECT 'internal-query'</sql></query></query-mapping>";

   function Loader (Name : in String) return access constant String is
   begin
      if Name = "regtests/files/internal-query.xml" then
         return Q1'Access;
      end if;

      return null;
   end Loader;

   --  ------------------------------
   --  Test the static query loader.
   --  ------------------------------
   procedure Test_Query_Loader (T : in out Test) is
      Query   : ADO.Queries.Context;
      Manager : Query_Manager;
      Config  : ADO.Connections.Configuration;
      Config_URL : constant String := Util.Tests.Get_Parameter ("test.database",
                                                                "sqlite:///regtests.db");
   begin
      Config.Set_Connection (Config_URL);
      Manager.Set_Query_Loader (Loader'Access);
      ADO.Queries.Loaders.Initialize (Manager, Config);
      Query.Set_Query ("internal-query");

      Assert_Equals (T, "SELECT 'internal-query'", Query.Get_SQL (Manager));

   end Test_Query_Loader;

end ADO.Queries.Tests;
