-----------------------------------------------------------------------
--  schemas Tests -- Test loading of database schema for MySQL
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with ADO.Drivers;
with ADO.Drivers.Connections;
with ADO.Sessions;
with ADO.Databases;
with ADO.Schemas.Mysql;

with Regtests;
package body ADO.Schemas.Mysql.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "ADO.Schemas.MySQL");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Schemas.Load_Schema",
                       Test_Load_Schema'Access);
   end Add_Tests;

   procedure Test_Load_Schema (T : in out Test) is
      use type ADO.Drivers.Connections.Driver_Access;

      S   : constant ADO.Sessions.Session := Regtests.Get_Database;
      DB  : constant ADO.Databases.Connection'Class := S.Get_Connection;
      Dr  : constant ADO.Drivers.Connections.Driver_Access := DB.Get_Driver;

      Schema : Schema_Definition;
      Table  : Table_Definition;
   begin
      T.Assert (Dr /= null, "Database connection has no driver");
      if Dr.Get_Driver_Name /= "mysql" then
         return;
      end if;

      ADO.Schemas.Mysql.Load_Schema (DB, Schema);

      Table := ADO.Schemas.Find_Table (Schema, "allocate");
      T.Assert (Table /= null, "Table schema for test_allocate not found");

      Assert_Equals (T, "allocate", Get_Name (Table));

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
         C : constant Column_Definition := Find_Column (Table, "ID");
      begin
         T.Assert (C /= null, "Cannot find column 'id' in table schema");
         Assert_Equals (T, "ID", Get_Name (C), "Invalid column name");
      end;
   end Test_Load_Schema;

end ADO.Schemas.Mysql.Tests;
