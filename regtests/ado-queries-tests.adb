-----------------------------------------------------------------------
--  ado-queries-tests -- Test loading of database queries
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2017 Stephane Carrez
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

with ADO.Drivers.Connections;
with ADO.Queries.Loaders;
package body ADO.Queries.Tests is

   use Util.Tests;

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
      use type ADO.Drivers.Driver_Index;

      Mysql_Driver  : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("mysql");
      Sqlite_Driver : constant Driver_Access := ADO.Drivers.Connections.Get_Driver ("sqlite");
      Config        : ADO.Drivers.Connections.Configuration;
      Manager       : Query_Manager;
   begin
      --  Configure the XML query loader.
      ADO.Queries.Loaders.Initialize (Manager, Config);

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Simple_Query.Query'Access, Manager, False);
      begin
         Assert_Equals (T, "select count(*) from user", SQL, "Invalid query for 'simple-query'");
      end;

      declare
         SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access, Manager, False);
      begin
         Assert_Equals (T, "select 0", SQL, "Invalid query for 'index'");
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
      if Sqlite_Driver /= null then
         declare
            SQL : constant String := ADO.Queries.Get_SQL (Index_Query.Query'Access,
                                                          Manager, False);
         begin
            Assert_Equals (T, "select 0", SQL, "Invalid query for 'index' (SQLite driver)");
         end;
      end if;
   end Test_Load_Queries;

   --  ------------------------------
   --  Test re-loading queries.
   --  ------------------------------
   procedure Test_Reload_Queries (T : in out Test) is
      Config  : ADO.Drivers.Connections.Configuration;
      Manager : Query_Manager;
      Query   : ADO.Queries.Context;
   begin
      --  Configure the XML query loader.
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
      Config  : ADO.Drivers.Connections.Configuration;
      Manager : Query_Manager;
   begin
      --  Configure and load the XML queries.
      for Pass in 1 .. 10 loop
         Config.Set_Property ("ado.queries.load", (if Pass = 1 then "false" else "true"));
         ADO.Queries.Loaders.Initialize (Manager, Config);
         T.Assert (Manager.Queries /= null, "The queries table is allocated");
         T.Assert (Manager.Files /= null, "The files table is allocated");
         for Query of Manager.Queries.all loop
            if Pass = 1 then
               T.Assert (Query.Is_Null, "Query must not be loaded");
            else
               T.Assert (not Query.Is_Null, "Query must have been loaded");
            end if;
         end loop;
      end loop;

   end Test_Initialize;

   --  ------------------------------
   --  Test the Set_Query operation.
   --  ------------------------------
   procedure Test_Set_Query (T : in out Test) is
      Query   : ADO.Queries.Context;
      Manager : Query_Manager;
      Config  : ADO.Drivers.Connections.Configuration;
   begin
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

end ADO.Queries.Tests;
