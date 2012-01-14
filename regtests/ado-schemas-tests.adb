-----------------------------------------------------------------------
--  schemas Tests -- Test loading of database schema
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

with ADO.Sessions;
with ADO.Schemas.Entities;
with ADO.Objects;

with Regtests;
with Regtests.Simple.Model;
package body ADO.Schemas.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "ADO.Schemas");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Schemas.Entities.Find_Entity_Type",
                       Test_Find_Entity_Type'Access);
      Caller.Add_Test (Suite, "Test ADO.Schemas.Entities.Find_Entity_Type (error)",
                       Test_Find_Entity_Type_Error'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test reading the entity cache and the Find_Entity_Type operation
   --  ------------------------------
   procedure Test_Find_Entity_Type (T : in out Test) is
      S   : ADO.Sessions.Session := Regtests.Get_Database;
      C   : ADO.Schemas.Entities.Entity_Cache;
   begin
      ADO.Schemas.Entities.Initialize (Cache   => C,
                                       Session => S);

      declare
--           T1 : constant ADO.Model.Entity_Type_Ref
--             := Entities.Find_Entity_Type (Cache => C,
--                                           Table => Regtests.Simple.Model.USER_TABLE'Access);
--           T2 : constant ADO.Model.Entity_Type_Ref
--             := Entities.Find_Entity_Type (Cache => C,
--                                           Table => Regtests.Simple.Model.ALLOCATE_TABLE'Access);

         T4 : constant ADO.Entity_Type
           := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.ALLOCATE_TABLE'Access);
         T5 : constant ADO.Entity_Type
           := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.USER_TABLE'Access);
      begin
--           T.Assert (not ADO.Objects.Is_Null (T1), "Find_Entity_Type returned a null value");
--           T.Assert (not ADO.Objects.Is_Null (T2), "Find_Entity_Type returned a null value");

         T.Assert (T4 /= T5, "Two distinct tables have different entity types");
         T.Assert (T4 > 0, "T1.Id must be positive");
         T.Assert (T5 > 0, "T2.Id must be positive");
--           T.Assert (T1.Get_Id /= T2.Get_Id, "Two distinct tables have different ids");
--
--           Assert_Equals (T, Integer (T2.Get_Id), Integer (T4),
--                          "Invalid entity type for allocate_table");
--           Assert_Equals (T, Integer (T1.Get_Id), Integer (T5),
--                          "Invalid entity type for user_table");
      end;
   end Test_Find_Entity_Type;

   --  ------------------------------
   --  Test calling Find_Entity_Type with an invalid table.
   --  ------------------------------
   procedure Test_Find_Entity_Type_Error (T : in out Test) is
      C   : ADO.Schemas.Entities.Entity_Cache;
   begin
      declare
         R : ADO.Entity_Type;
         pragma Unreferenced (R);
      begin
         R := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.USER_TABLE'Access);
         T.Assert (False, "Find_Entity_Type did not raise the No_Entity_Type exception");

      exception
         when ADO.Schemas.Entities.No_Entity_Type =>
            null;
      end;
   end Test_Find_Entity_Type_Error;

end ADO.Schemas.Tests;
