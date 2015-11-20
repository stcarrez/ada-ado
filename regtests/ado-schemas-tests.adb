-----------------------------------------------------------------------
--  schemas Tests -- Test loading of database schema
--  Copyright (C) 2009, 2010, 2011, 2012, 2015 Stephane Carrez
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
with ADO.Sessions;
with ADO.Schemas.Entities;

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
      Caller.Add_Test (Suite, "Test ADO.Sessions.Load_Schema",
                       Test_Load_Schema'Access);
      Caller.Add_Test (Suite, "Test ADO.Schemas.Get_Table",
                       Test_Table_Iterator'Access);
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
                                         Table => Regtests.Simple.Model.ALLOCATE_TABLE);
         T5 : constant ADO.Entity_Type
           := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.USER_TABLE);
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
                                         Table => Regtests.Simple.Model.USER_TABLE);
         T.Assert (False, "Find_Entity_Type did not raise the No_Entity_Type exception");

      exception
         when ADO.Schemas.Entities.No_Entity_Type =>
            null;
      end;
   end Test_Find_Entity_Type_Error;

   --  ------------------------------
   --  Test the Load_Schema operation and check the result schema.
   --  ------------------------------
   procedure Test_Load_Schema (T : in out Test) is
      S      : constant ADO.Sessions.Session := Regtests.Get_Database;
      Schema : Schema_Definition;
      Table  : Table_Definition;
   begin
      S.Load_Schema (Schema);

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
         T.Assert (Get_Type (C) = T_LONG_INTEGER, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "NAME");
      begin
         T.Assert (C /= null, "Cannot find column 'NAME' in table schema");
         Assert_Equals (T, "NAME", Get_Name (C), "Invalid column name");
         T.Assert (Get_Type (C) = T_VARCHAR, "Invalid column type");
         T.Assert (Is_Null (C), "Column is null");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "version");
      begin
         T.Assert (C /= null, "Cannot find column 'version' in table schema");
         Assert_Equals (T, "version", Get_Name (C), "Invalid column name");
         T.Assert (Get_Type (C) = T_INTEGER, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "this_column_does_not_exist");
      begin
         T.Assert (C = null, "Find_Column must return null for an unknown column");
      end;
   end Test_Load_Schema;

   --  ------------------------------
   --  Test the Table_Cursor operations and check the result schema.
   --  ------------------------------
   procedure Test_Table_Iterator (T : in out Test) is
      S      : constant ADO.Sessions.Session := Regtests.Get_Database;
      Schema : Schema_Definition;
      Table  : Table_Definition;
   begin
      S.Load_Schema (Schema);

      declare
         Iter  : Table_Cursor := Schema.Get_Tables;
         Count : Natural := 0;
      begin
         T.Assert (Has_Element (Iter), "The Get_Tables returns an empty iterator");
         while Has_Element (Iter) loop
            Count := Count + 1;
            Next (Iter);
         end loop;
         Util.Tests.Assert_Equals (T, 15, Count, "Invalid number of tables found in the schema");
      end;
   end Test_Table_Iterator;

end ADO.Schemas.Tests;
