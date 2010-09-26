-----------------------------------------------------------------------
--  schemas Tests -- Test loading of database schema
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AUnit.Test_Caller;
with AUnit.Test_Fixtures;
with AUnit.Assertions;

with Util.Tests;
with ADO.Sessions;
with ADO.Databases;
with ADO.Schemas.Mysql;
with Regtests;
package body ADO.Schemas.Tests is

   use AUnit.Assertions;
   use Util.Tests;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test ADO.Schemas.Load_Schema",
        Test_Load_Schema'Access));
   end Add_Tests;

   procedure Test_Load_Schema (T : in out Test) is

      S   : ADO.Sessions.Session := Regtests.Get_Database;
      DB  : ADO.Databases.Connection'Class := S.Get_Connection;

      Schema : Schema_Definition;
      Table  : Table_Definition;
   begin
      ADO.Schemas.Mysql.Load_Schema (DB, Schema);

      Table := ADO.Schemas.Find_Table (Schema, "test_allocate");
      Assert (Table /= null, "Table schema for test_allocate not found");

      Assert_Equals (T, "test_allocate", Get_Name (Table));

      declare
         C : Column_Cursor := Get_Columns (Table);
         Nb_Columns : Integer := 0;
      begin
         while Has_Element (C) loop
            Nb_Columns := Nb_Columns + 1;
            Next (C);
         end loop;
         Assert_Equals (T, 3, Nb_Columns, "Invalid number of columns");
      end;

      declare
         C : Column_Definition := Find_Column (Table, "id");
      begin
         T.Assert (C /= null, "Cannot find column 'id' in table schema");
         Assert_Equals (T, "id", Get_Name (C), "Invalid column name");
      end;
   end Test_Load_Schema;

end ADO.Schemas.Tests;
