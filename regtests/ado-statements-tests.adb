-----------------------------------------------------------------------
--  ado-statements-tests -- Test statements package
--  Copyright (C) 2015, 2017 Stephane Carrez
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
with Util.Strings.Transforms;

with ADO.Utils;
with ADO.Sessions;

with Regtests.Statements.Model;
package body ADO.Statements.Tests is

   use Util.Tests;

   procedure Populate (Tst : in out Test);
   function Get_Sum (T     : in Test;
                     Table : in String) return Natural;
   function Get_Sum (T     : in Test;
                     Table : in String;
                     Ids   : in ADO.Utils.Identifier_Vector) return Natural;

   --  Test the query statement Get_Xxx operation for various types.
   generic
      type T (<>) is private;
      with function Get_Value (Stmt   : in ADO.Statements.Query_Statement;
                               Column : in Natural) return T is <>;
      Name   : String;
      Column : String;
   procedure Test_Query_Get_Value_T (Tst   : in out Test);

   procedure Populate (Tst : in out Test) is
      DB    : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
   begin
      DB.Begin_Transaction;
      for I in 1 .. 10 loop
         declare
            Item : Regtests.Statements.Model.Table_Ref;
         begin
            Item.Set_Id_Value (ADO.Identifier (I * I));
            Item.Set_Int_Value (I);
            Item.Set_Bool_Value ((I mod 2) = 0);
            Item.Set_String_Value ("Item" & Integer'Image (I));
            Item.Set_Time_Value (Ada.Calendar.Clock);
            Item.Set_Entity_Value (ADO.Entity_Type (10 - I));
            Item.Save (DB);
            Tst.Assert (Item.Is_Inserted, "Item inserted in database");
         end;
      end loop;
      DB.Commit;
   end Populate;

   --  ------------------------------
   --  Test the query statement Get_Xxx operation for various types.
   --  ------------------------------
   procedure Test_Query_Get_Value_T (Tst   : in out Test) is
      Stmt : ADO.Statements.Query_Statement;
      DB   : constant ADO.Sessions.Session := Regtests.Get_Database;
   begin
      Populate (Tst);

      --  Check that Get_Value raises an exception if the statement is invalid.
      begin
         declare
            V    : T := Get_Value (Stmt, 0);
            pragma Unreferenced (V);
         begin
            Util.Tests.Fail (Tst, "No Invalid_Statement exception raised for " & Name);
         end;

      exception
         when Invalid_Statement =>
            null;
      end;

      --  Execute a query to fetch one column.
      Stmt := DB.Create_Statement ("SELECT " & Column & " FROM test_table WHERE id = 1");
      Stmt.Execute;

      --  Verify the query result and the Get_Value operation.
      Tst.Assert (Stmt.Has_Elements, "The query statement must return a value for "
                  & Name & ":" & Column);
      Tst.Assert (not Stmt.Is_Null (0), "The query statement must return a non null value for "
                  & Name & ":" & Column);
      Util.Tests.Assert_Equals (Tst, Column,
                                Util.Strings.Transforms.To_Lower_Case (Stmt.Get_Column_Name (0)),
                                "The query returns an invalid column name");
      declare
         V : T := Get_Value (Stmt, 0);
         pragma Unreferenced (V);
      begin
         Stmt.Clear;
      end;

   end Test_Query_Get_Value_T;

   procedure Test_Query_Get_Int64 is
     new Test_Query_Get_Value_T (Int64, ADO.Statements.Get_Int64, "Get_Int64", "id_value");

   procedure Test_Query_Get_Integer is
     new Test_Query_Get_Value_T (Integer, ADO.Statements.Get_Integer, "Get_Integer", "int_value");

   procedure Test_Query_Get_Nullable_Integer is
     new Test_Query_Get_Value_T (Nullable_Integer, ADO.Statements.Get_Nullable_Integer,
                                 "Get_Nullable_Integer", "int_value");

   procedure Test_Query_Get_Nullable_Entity_Type is
     new Test_Query_Get_Value_T (Nullable_Entity_Type, ADO.Statements.Get_Nullable_Entity_Type,
                                 "Get_Nullable_Entity_Type", "entity_value");

   procedure Test_Query_Get_Natural is
     new Test_Query_Get_Value_T (Natural, ADO.Statements.Get_Natural, "Get_Natural", "int_value");

   procedure Test_Query_Get_Identifier is
     new Test_Query_Get_Value_T (ADO.Identifier, ADO.Statements.Get_Identifier,
                                 "Get_Identifier", "id_value");

   procedure Test_Query_Get_Boolean is
     new Test_Query_Get_Value_T (Boolean, ADO.Statements.Get_Boolean, "Get_Boolean", "bool_value");

   procedure Test_Query_Get_String is
     new Test_Query_Get_Value_T (String, ADO.Statements.Get_String, "Get_String", "string_value");

   package Caller is new Util.Test_Caller (Test, "ADO.Statements");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Statements.Save",
                       Test_Save'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Int64",
                       Test_Query_Get_Int64'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Integer",
                       Test_Query_Get_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Nullable_Integer",
                       Test_Query_Get_Nullable_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Natural",
                       Test_Query_Get_Natural'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Identifier",
                       Test_Query_Get_Identifier'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Boolean",
                       Test_Query_Get_Boolean'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_String",
                       Test_Query_Get_String'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Get_Nullable_Entity_Type",
                       Test_Query_Get_Nullable_Entity_Type'Access);
      Caller.Add_Test (Suite, "Test ADO.Statements.Create_Statement (using $entity_type[])",
                       Test_Entity_Types'Access);
   end Add_Tests;

   function Get_Sum (T : in Test;
                     Table : in String) return Natural is
      DB   : constant ADO.Sessions.Session := Regtests.Get_Database;
      Stmt : ADO.Statements.Query_Statement := DB.Create_Statement ("SELECT SUM(id_value) FROM "
                                                                    & Table);
   begin
      Stmt.Execute;
      T.Assert (Stmt.Has_Elements, "The query statement must return a value for table " & Table);
      return Stmt.Get_Integer (0);
   end Get_Sum;

   function Get_Sum (T     : in Test;
                     Table : in String;
                     Ids   : in ADO.Utils.Identifier_Vector) return Natural is
      DB   : constant ADO.Sessions.Session := Regtests.Get_Database;
      Stmt : ADO.Statements.Query_Statement := DB.Create_Statement ("SELECT SUM(id_value) FROM "
                                                                    & Table
                                                                    & " WHERE id IN (:ids)");
   begin
      Stmt.Bind_Param ("ids", Ids);
      Stmt.Execute;
      T.Assert (Stmt.Has_Elements, "The query statement must return a value for table " & Table);
      return Stmt.Get_Integer (0);
   end Get_Sum;

   --  ------------------------------
   --  Test creation of several rows in test_table with different column type.
   --  ------------------------------
   procedure Test_Save (T : in out Test) is
      First  : constant Natural := Get_Sum (T, "test_table");
      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      List   : ADO.Utils.Identifier_Vector;
   begin
      DB.Begin_Transaction;
      for I in 1 .. 10 loop
         declare
            Item : Regtests.Statements.Model.Table_Ref;
         begin
            Item.Set_Id_Value (ADO.Identifier (I * I));
            Item.Set_Int_Value (I);
            Item.Set_Bool_Value ((I mod 2) = 0);
            Item.Set_String_Value ("Item" & Integer'Image (I));
            Item.Set_Time_Value (Ada.Calendar.Clock);
            Item.Set_Entity_Value (ADO.Entity_Type (10 - I));
            Item.Save (DB);
            List.Append (Item.Get_Id);
         end;
      end loop;
      DB.Commit;

      Util.Tests.Assert_Equals (T, First + 385, Get_Sum (T, "test_table"),
                                "The SUM query returns an invalid value for test_table");
      Util.Tests.Assert_Equals (T, 385, Get_Sum (T, "test_table", List),
                                "The SUM query returns an invalid value for test_table");
   end Test_Save;

   --  ------------------------------
   --  Test queries using the $entity_type[] cache group.
   --  ------------------------------
   procedure Test_Entity_Types (T : in out Test) is
      DB    : constant ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Stmt  : ADO.Statements.Query_Statement;
      Count : Natural := 0;
   begin
      Stmt := DB.Create_Statement ("SELECT name FROM entity_type "
                                   & "WHERE entity_type.id = $entity_type[test_user]");
      Stmt.Execute;
      while Stmt.Has_Elements loop
         Util.Tests.Assert_Equals (T, "test_user", Stmt.Get_String (0), "Invalid query response");
         Count := Count + 1;
         Stmt.Next;
      end loop;
      Util.Tests.Assert_Equals (T, 1, Count, "Query must return one row");
   end Test_Entity_Types;

end ADO.Statements.Tests;
