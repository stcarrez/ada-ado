-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
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

with ADO.Testsuite;
with ADO.Drivers.Mysql;
with Regtests;

with Util.Tests;
with Util.Properties;
procedure ADO_Mysql_Harness is

   procedure Initialize (Props : in Util.Properties.Manager);

   procedure Harness is new Util.Tests.Harness (ADO.Testsuite.Suite,
                                               Initialize);

   --  ------------------------------
   --  Initialization procedure: setup the database
   --  ------------------------------
   procedure Initialize (Props : in Util.Properties.Manager) is
      DB : constant String := Props.Get ("test.database",
                                         "mysql://localhost:3306/ado?user=testado&password=ado");
   begin
      ADO.Drivers.Mysql.Initialize;
      Regtests.Initialize (DB);
   end Initialize;

begin
   Harness ("ado-tests.xml");
end ADO_Mysql_Harness;
