-----------------------------------------------------------------------
--  schemas Tests -- Test loading of database schema
--  Copyright (C) 2009, 2010, 2011, 2015 Stephane Carrez
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

with Util.Tests;
package ADO.Schemas.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test reading the entity cache and the Find_Entity_Type operation
   procedure Test_Find_Entity_Type (T : in out Test);

   --  Test calling Find_Entity_Type with an invalid table.
   procedure Test_Find_Entity_Type_Error (T : in out Test);

   --  Test the Load_Schema operation and check the result schema.
   procedure Test_Load_Schema (T : in out Test);

end ADO.Schemas.Tests;
