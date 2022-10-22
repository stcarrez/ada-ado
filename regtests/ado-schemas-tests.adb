-----------------------------------------------------------------------
--  ado-schemas-tests -- Test loading of database schema
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with Ada.Directories;
with Ada.Text_IO;

with Util.Test_Caller;
with Util.Strings.Vectors;
with Util.Strings.Transforms;

with ADO.Parameters;
with ADO.Schemas.Databases;
with ADO.Sessions.Sources;
with ADO.Sessions.Entities;
with ADO.Schemas.Entities;

with Regtests;
with Regtests.Audits.Model;
with Regtests.Simple.Model;
package body ADO.Schemas.Tests is

   use Util.Tests;

   function To_Lower_Case (S : in String) return String
     renames Util.Strings.Transforms.To_Lower_Case;

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
      Caller.Add_Test (Suite, "Test ADO.Schemas.Get_Table (Empty schema)",
                       Test_Empty_Schema'Access);
      Caller.Add_Test (Suite, "Test ADO.Schemas.Databases.Create_Database",
                       Test_Create_Schema'Access);
      Caller.Add_Test (Suite, "Test ADO.Schemas.Databases.Sort_Migration",
                       Test_Sort_Migration'Access);
      Caller.Add_Test (Suite, "Test ADO.Schemas.Databases.Scan_Migration",
                       Test_Scan_Migration'Access);
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
         T4 : constant ADO.Entity_Type
           := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.ALLOCATE_TABLE);
         T5 : constant ADO.Entity_Type
           := Entities.Find_Entity_Type (Cache => C,
                                         Table => Regtests.Simple.Model.USER_TABLE);
         T1 : constant ADO.Entity_Type
           := Sessions.Entities.Find_Entity_Type (Session => S,
                                                  Table   => Regtests.Audits.Model.AUDIT_TABLE);
         T2 : constant ADO.Entity_Type
           := Sessions.Entities.Find_Entity_Type (Session => S,
                                                  Table   => Regtests.Audits.Model.EMAIL_TABLE);
         T3 : constant ADO.Entity_Type
           := Sessions.Entities.Find_Entity_Type (Session => S,
                                                  Name    => "audit_property");
      begin
         T.Assert (T1 > 0, "T1 must be positive");
         T.Assert (T2 > 0, "T2 must be positive");
         T.Assert (T3 > 0, "T3 must be positive");
         T.Assert (T4 > 0, "T4 must be positive");
         T.Assert (T5 > 0, "T5 must be positive");

         T.Assert (T1 /= T2, "Two distinct tables have different entity types (T1, T2)");
         T.Assert (T2 /= T3, "Two distinct tables have different entity types (T2, T3)");
         T.Assert (T3 /= T4, "Two distinct tables have different entity types (T3, T4)");
         T.Assert (T4 /= T5, "Two distinct tables have different entity types (T4, T5)");
         T.Assert (T5 /= T1, "Two distinct tables have different entity types (T5, T1)");
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

      declare
         P : ADO.Parameters.Parameter
           := ADO.Parameters.Parameter'(T => ADO.Parameters.T_INTEGER, Num => 0,
                                        Len => 0, Value_Len => 0, Position => 0, Name => "");
      begin
         P := C.Expand ("something");
         T.Assert (False, "Expand did not raise the No_Entity_Type exception");

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
         Assert_Equals (T, "id", To_Lower_Case (Get_Name (C)), "Invalid column name");
         T.Assert (Get_Type (C) = T_LONG_INTEGER, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
         T.Assert (Is_Primary (C), "Column must be a primary key");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "NAME");
      begin
         T.Assert (C /= null, "Cannot find column 'NAME' in table schema");
         Assert_Equals (T, "name", To_Lower_Case (Get_Name (C)), "Invalid column name");
         T.Assert (Get_Type (C) = T_VARCHAR, "Invalid column type");
         T.Assert (Is_Null (C), "Column is null");
         T.Assert (not Is_Primary (C), "Column must not be a primary key");
         Assert_Equals (T, 255, Get_Size (C), "Column has invalid size");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "version");
      begin
         T.Assert (C /= null, "Cannot find column 'version' in table schema");
         Assert_Equals (T, "version", To_Lower_Case (Get_Name (C)), "Invalid column name");
         T.Assert (Get_Type (C) = T_INTEGER, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
         Assert_Equals (T, "", Get_Default (C), "Column has a default value");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "this_column_does_not_exist");
      begin
         T.Assert (C = null, "Find_Column must return null for an unknown column");
      end;

      Table := ADO.Schemas.Find_Table (Schema, "audit_property");
      T.Assert (Table /= null, "Table schema for audit_property not found");

      Assert_Equals (T, "audit_property", Get_Name (Table));
      declare
         C : Column_Cursor := Get_Columns (Table);
         Nb_Columns : Integer := 0;
      begin
         while Has_Element (C) loop
            Nb_Columns := Nb_Columns + 1;
            Next (C);
         end loop;
         Assert_Equals (T, 7, Nb_Columns, "Invalid number of columns");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "float_value");
      begin
         T.Assert (C /= null, "Cannot find column 'float_value' in table schema");
         Assert_Equals (T, "float_value", To_Lower_Case (Get_Name (C)), "Invalid column name");
         T.Assert (Get_Type (C) = T_FLOAT, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
         T.Assert (not Is_Binary (C), "Column is binary");
         T.Assert (not Is_Primary (C), "Column must be not be a primary key");
      end;

      declare
         C : constant Column_Definition := Find_Column (Table, "double_value");
      begin
         T.Assert (C /= null, "Cannot find column 'double_value' in table schema");
         Assert_Equals (T, "double_value", To_Lower_Case (Get_Name (C)), "Invalid column name");
         T.Assert (Get_Type (C) = T_DOUBLE, "Invalid column type");
         T.Assert (not Is_Null (C), "Column is null");
         T.Assert (not Is_Binary (C), "Column is binary");
         T.Assert (not Is_Primary (C), "Column must be not be a primary key");
      end;

   end Test_Load_Schema;

   --  ------------------------------
   --  Test the Table_Cursor operations and check the result schema.
   --  ------------------------------
   procedure Test_Table_Iterator (T : in out Test) is
      S      : constant ADO.Sessions.Session := Regtests.Get_Database;
      Schema : Schema_Definition;
      Table  : Table_Definition;
      Driver : constant String := S.Get_Driver.Get_Driver_Name;
   begin
      S.Load_Schema (Schema);

      declare
         Iter  : Table_Cursor := Schema.Get_Tables;
         Count : Natural := 0;
      begin
         T.Assert (Has_Element (Iter), "The Get_Tables returns an empty iterator");
         while Has_Element (Iter) loop
            Table := Element (Iter);
            T.Assert (Table /= null, "Element function must not return null");
            declare
               Col_Iter : Column_Cursor := Get_Columns (Table);
            begin
               --  T.Assert (Has_Element (Col_Iter), "Table has a column");
               while Has_Element (Col_Iter) loop
                  T.Assert (Element (Col_Iter) /= null, "Element function must not return null");
                  Next (Col_Iter);
               end loop;
            end;
            Count := Count + 1;
            Next (Iter);
         end loop;
         if Driver = "sqlite" then
            Util.Tests.Assert_Equals (T, 13, Count,
                                      "Invalid number of tables found in the schema");
         elsif Driver = "mysql" then
            Util.Tests.Assert_Equals (T, 13, Count,
                                      "Invalid number of tables found in the schema");
         elsif Driver = "postgresql" then
            Util.Tests.Assert_Equals (T, 13, Count,
                                      "Invalid number of tables found in the schema");
         end if;
      end;
   end Test_Table_Iterator;

   --  ------------------------------
   --  Test the Table_Cursor operations on an empty schema.
   --  ------------------------------
   procedure Test_Empty_Schema (T : in out Test) is
      Schema : Schema_Definition;
      Iter   : constant Table_Cursor := Schema.Get_Tables;
   begin
      T.Assert (not Has_Element (Iter), "The Get_Tables must return an empty iterator");
      T.Assert (Schema.Find_Table ("test") = null, "The Find_Table must return null");
   end Test_Empty_Schema;

   --  ------------------------------
   --  Test the creation of database.
   --  ------------------------------
   procedure Test_Create_Schema (T : in out Test) is
      use ADO.Sessions.Sources;

      Unused_Msg : Util.Strings.Vectors.Vector;
      Cfg      : Data_Source := Data_Source (Regtests.Get_Controller);
      Driver   : constant String := Cfg.Get_Driver;
      Database : constant String := Cfg.Get_Database;
      Path     : constant String :=
        "db/regtests/" & Cfg.Get_Driver & "/create-ado-" & Driver & ".sql";
   begin
      if Driver = "sqlite" then
         if Ada.Directories.Exists (Database & ".test") then
            Ada.Directories.Delete_File (Database & ".test");
         end if;
         Cfg.Set_Database (Database & ".test");
         ADO.Schemas.Databases.Create_Database (Admin       => Cfg,
                                                Config      => Cfg,
                                                Schema_Path => Path,
                                                Messages    => Unused_Msg);
         T.Assert (Ada.Directories.Exists (Database & ".test"),
                   "The sqlite database was not created");
      else
         ADO.Schemas.Databases.Create_Database (Admin       => Cfg,
                                                Config      => Cfg,
                                                Schema_Path => Path,
                                                Messages    => Unused_Msg);
      end if;
   end Test_Create_Schema;


   --  ------------------------------
   --  Test the sort migration.
   --  ------------------------------
   procedure Test_Sort_Migration (T : in out Test) is
      use ADO.Schemas.Databases;

      function To_Ustring (Value : in String) return Ada.Strings.Unbounded.Unbounded_String
        is (Ada.Strings.Unbounded.To_Unbounded_String (Value));

      function To_String (Value : in Upgrade_Type) return String
        is (To_String (Value.Name) & ":"  & Value.Version'Image & "("
              & To_String (Value.Path) & ") [" & To_String (Value.Depend) & "]");

      Ado_1   : constant Upgrade_Type := (Version => 1,
                                          Name    => To_Ustring ("ado"),
                                          Depend  => To_Ustring (""),
                                          Path    => To_Ustring ("1"));
      Awa_1   : constant Upgrade_Type := (Version => 1,
                                          Name    => To_Ustring ("awa"),
                                          Depend  => To_Ustring ("ado:1"),
                                          Path    => To_Ustring ("2"));
      Blog_1  : constant Upgrade_Type := (Version => 1,
                                          Name    => To_Ustring ("awa-blogs"),
                                          Depend  => To_Ustring ("awa:1 ado:1"),
                                          Path    => To_Ustring ("3"));
      Ado_2   : constant Upgrade_Type := (Version => 2,
                                          Name    => To_Ustring ("ado"),
                                          Depend  => To_Ustring (""),
                                          Path    => To_Ustring ("4"));
      Awa_2   : constant Upgrade_Type := (Version => 2,
                                          Name    => To_Ustring ("awa"),
                                          Depend  => To_Ustring ("ado:2"),
                                          Path    => To_Ustring ("5"));
      Blog_2  : constant Upgrade_Type := (Version => 2,
                                          Name    => To_Ustring ("awa-blogs"),
                                          Depend  => To_Ustring ("awa:2 ado:2"),
                                          Path    => To_Ustring ("6"));
      List    : ADO.Schemas.Databases.Upgrade_List;
   begin
      List.Append (Ado_1);
      List.Append (Awa_1);
      List.Append (Blog_1);
      List.Append (Ado_2);
      List.Append (Awa_2);
      List.Append (Blog_2);
      ADO.Schemas.Databases.Upgrade_Lists.Reverse_Elements (List);

      T.Assert (Ado_1 < Ado_2, "ado:1 < ado:2");
      T.Assert (not (Ado_2 < Ado_1), "not ado:2 < ado:1");
      T.Assert (Ado_1 < Awa_1, "ado:1 < awa:1");
      T.Assert (not (Awa_1 < Ado_1), "not awa:1 < ado:1");
      T.Assert (Ado_1 < Blog_1, "ado:1 < blog:1");
      T.Assert (not (Blog_1 < Ado_1), "not blog:1 < ado:1");
      T.Assert (Ado_1 < Blog_2, "ado:1 < blog:2");
      T.Assert (not (Blog_2 < Ado_1), "not blog:2 < ado:1");
      T.Assert (Blog_1 < Ado_2, "blog:1 < ado:2");
      T.Assert (not (Ado_2 < Blog_1), "not ado:2 < blog:1");
      T.Assert (Blog_1 < Blog_2, "blog:1 < blog:2");
      T.Assert (not (Blog_2 < Blog_1), "not blog:2 < blog:1");
      T.Assert (Blog_1 < Awa_2, "blog:1 < awa:2");
      T.Assert (not (Awa_2 < Blog_1), "not awa:2 < blog:1");
      T.Assert (Ado_1 < Awa_2, "ado:1 < awa:2");
      T.Assert (not (Awa_2 < Ado_1), "not awa:2 < ado:1");
      T.Assert (Ado_2 < Awa_2, "ado:2 < awa:2");
      T.Assert (not (Awa_2 < Ado_2), "not awa:2 < ado:2");
      T.Assert (Awa_2 < Blog_2, "awa:2 < blog:2");
      T.Assert (not (Blog_2 < Awa_2), "not blog:2 < awa:2");

      ADO.Schemas.Databases.Sort_Migration (List);

      Ada.Text_IO.Put_Line ("Sorted:");
      declare
         I : Natural := 0;
         J : Natural := 0;
      begin
         for Right of List loop
            I := I + 1;
            J := 0;
            for Left of List loop
               J := J + 1;
               exit when J > I;
               if I = J then
                  T.Assert (not (Left < Right), "Invalid comparison");
               elsif Left < Right then
                  Ada.Text_Io.Put_Line (To_String (Left) & " < " & To_String (Right));
               else
                  Ada.Text_Io.Put_Line ("ERROR: " & To_String (Left) & " < " & To_String (Right));
                  T.Assert (Left < Right, "Invalid comparison");
               end if;
            end loop;
         end loop;
      end;
   end Test_Sort_Migration;

   --  ------------------------------
   --  Test the scan of migration.
   --  ------------------------------
   procedure Test_Scan_Migration (T : in out Test) is
      Path    : constant String := Util.Tests.Get_Path ("regtests/files/migration1");
      S       : constant ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      List    : ADO.Schemas.Databases.Upgrade_List;
   begin
      --  Cleanup the ado_version table to force a migration for the unit test.
      S.Execute ("DELETE FROM ado_version WHERE name = 'awa'");
      S.Execute ("DELETE FROM ado_version WHERE name = 'awa-blogs'");
      S.Execute ("UPDATE ado_version SET version = 0 WHERE name = 'ado'");

      ADO.Schemas.Databases.Scan_Migration (S, Path, List);
      for Upgrade of List loop
         Ada.Text_IO.Put_Line (To_String (Upgrade.Name) & " => "
                               & To_String (Upgrade.Path) & " " & Upgrade.Version'Image);
      end loop;

      ADO.Schemas.Databases.Sort_Migration (List);

      Ada.Text_IO.Put_Line ("Sorted:");
      for Upgrade of List loop
         Ada.Text_IO.Put_Line (To_String (Upgrade.Name) & " => "
                               & To_String (Upgrade.Path) & " " & Upgrade.Version'Image);
      end loop;

      --  Reverse the order and sort again to make sure the Sort is correct.
      ADO.Schemas.Databases.Upgrade_Lists.Reverse_Elements (List);

      ADO.Schemas.Databases.Sort_Migration (List);

      Ada.Text_IO.Put_Line ("Sorted:");
      for Upgrade of List loop
         Ada.Text_IO.Put_Line (To_String (Upgrade.Name) & " => "
                                 & To_String (Upgrade.Path) & " " & Upgrade.Version'Image
                                 & " [" & To_String (Upgrade.Depend) & "]");
      end loop;

      Util.Tests.Assert_Equals (T, 6, Natural (List.Length), "Invalid number of upgrade");
      declare
         R : Unbounded_String;
      begin
         for Upgrade of List loop
            Append (R, Upgrade.Name);
            Append (R, Upgrade.Version'Image);
         end loop;
         Util.Tests.Assert_Equals
           (T, "ado 1awa 1awa-blogs 1ado 2awa 2awa-blogs 2",
            R, "Invalid migration order");
      end;

      declare
         Files : Util.Strings.Vectors.Vector;
      begin
         for Upgrade of List loop
            ADO.Schemas.Databases.Run_Migration
               (S, Upgrade, Files, ADO.Sessions.Execute'Access);
         end loop;
      end;

      --  Run again, we should find nothing in the list.
      List.Clear;
      ADO.Schemas.Databases.Scan_Migration (S, Path, List);
      Util.Tests.Assert_Equals (T, 0, Natural (List.Length), "Upgrade list must be empty");
   end Test_Scan_Migration;

end ADO.Schemas.Tests;
