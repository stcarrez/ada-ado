-----------------------------------------------------------------------
--  ado-parameters-tests -- Test query parameters and SQL expansion
--  Copyright (C) 2011, 2015, 2017 Stephane Carrez
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
with Util.Measures;
with ADO.Caches.Discrete;
with ADO.Caches;
package body ADO.Parameters.Tests is

   use Util.Tests;

   package Int_Cache is new ADO.Caches.Discrete (Element_Type => Integer);

   type Dialect is new ADO.Drivers.Dialects.Dialect with null record;

   --  Check if the string is a reserved keyword.
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean;

   --  Test the Add_Param operation for various types.
   generic
      type T (<>) is private;
      with procedure Add_Param (L : in out ADO.Parameters.Abstract_List;
                                V : in T) is <>;
      Value : T;
      Name  : String;
   procedure Test_Add_Param_T (Tst   : in out Test);

   --  Test the Bind_Param operation for various types.
   generic
      type T (<>) is private;
      with procedure Bind_Param (L : in out ADO.Parameters.Abstract_List;
                                 N : in String;
                                 V : in T) is <>;
      Value : T;
      Name  : String;
   procedure Test_Bind_Param_T (Tst   : in out Test);

   --  Test the Add_Param operation for various types.
   procedure Test_Add_Param_T (Tst   : in out Test) is
      Count : constant Positive := 100;
      SQL   : ADO.Parameters.List;
      S     : Util.Measures.Stamp;
   begin
      for I in 1 .. Count loop
         Add_Param (ADO.Parameters.Abstract_List (SQL), Value);
      end loop;
      Util.Measures.Report (S, "Add_Param " & Name & "(" & Positive'Image (Count) & " calls)");
      Util.Tests.Assert_Equals (Tst, Count, SQL.Length, "Invalid param list for " & Name);
   end Test_Add_Param_T;

   --  Test the Bind_Param operation for various types.
   procedure Test_Bind_Param_T (Tst   : in out Test) is
      Count : constant Positive := 100;
      SQL   : ADO.Parameters.List;
      S     : Util.Measures.Stamp;
   begin
      for I in 1 .. Count loop
         Bind_Param (ADO.Parameters.Abstract_List (SQL), "a_parameter_name", Value);
      end loop;
      Util.Measures.Report (S, "Bind_Param " & Name & "(" & Positive'Image (Count) & " calls)");
      Util.Tests.Assert_Equals (Tst, 1, SQL.Length, "Invalid param list for " & Name);
   end Test_Bind_Param_T;

   procedure Test_Add_Param_Integer is
     new Test_Add_Param_T (Integer, Add_Param, 10, "Integer");
   procedure Test_Add_Param_Long_Long_Integer is
     new Test_Add_Param_T (Long_Long_Integer, Add_Param, 10_000_000_000_000, "Long_Long_Integer");
   procedure Test_Add_Param_Identifier is
     new Test_Add_Param_T (Identifier, Add_Param, 100, "Identifier");
   procedure Test_Add_Param_Boolean is
     new Test_Add_Param_T (Boolean, Add_Param, False, "Boolean");
   procedure Test_Add_Param_String is
     new Test_Add_Param_T (String, Add_Param, "0123456789ABCDEF", "String");
   procedure Test_Add_Param_Calendar is
     new Test_Add_Param_T (Ada.Calendar.Time, Add_Param, Ada.Calendar.Clock, "Time");
   procedure Test_Add_Param_Unbounded_String is
     new Test_Add_Param_T (Unbounded_String, Add_Param, To_Unbounded_String ("0123456789ABCDEF"),
                           "Unbounded_String");

   procedure Test_Bind_Param_Integer is
     new Test_Bind_Param_T (Integer, Bind_Param, 10, "Integer");
   procedure Test_Bind_Param_Long_Long_Integer is
     new Test_Bind_Param_T (Long_Long_Integer, Bind_Param, 10_000_000_000_000,
                            "Long_Long_Integer");
   procedure Test_Bind_Param_Identifier is
     new Test_Bind_Param_T (Identifier, Bind_Param, 100, "Identifier");
   procedure Test_Bind_Param_Entity_Type is
     new Test_Bind_Param_T (Entity_Type, Bind_Param, 100, "Entity_Type");
   procedure Test_Bind_Param_Boolean is
     new Test_Bind_Param_T (Boolean, Bind_Param, False, "Boolean");
   procedure Test_Bind_Param_String is
     new Test_Bind_Param_T (String, Bind_Param, "0123456789ABCDEF", "String");
   procedure Test_Bind_Param_Calendar is
     new Test_Bind_Param_T (Ada.Calendar.Time, Bind_Param, Ada.Calendar.Clock, "Time");
   procedure Test_Bind_Param_Unbounded_String is
     new Test_Bind_Param_T (Unbounded_String, Bind_Param, To_Unbounded_String ("0123456789ABCDEF"),
                           "Unbounded_String");

   package Caller is new Util.Test_Caller (Test, "ADO.Parameters");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand",
                       Test_Expand_Sql'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand (invalid params)",
                       Test_Expand_Error'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand (perf)",
                       Test_Expand_Perf'Access);

      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Boolean)",
                       Test_Add_Param_Boolean'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Integer)",
                       Test_Add_Param_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Long_Long_Integer)",
                       Test_Add_Param_Long_Long_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Identifier)",
                       Test_Add_Param_Identifier'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Calendar)",
                       Test_Add_Param_Calendar'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (String)",
                       Test_Add_Param_String'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Add_Param (Unbounded_String)",
                       Test_Add_Param_Unbounded_String'Access);

      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Boolean)",
                       Test_Bind_Param_Boolean'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Integer)",
                       Test_Bind_Param_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Long_Long_Integer)",
                       Test_Bind_Param_Long_Long_Integer'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Identifier)",
                       Test_Bind_Param_Identifier'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Calendar)",
                       Test_Bind_Param_Calendar'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (String)",
                       Test_Bind_Param_String'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Unbounded_String)",
                       Test_Bind_Param_Unbounded_String'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Bind_Param (Entity_Type)",
                       Test_Bind_Param_Entity_Type'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand (cache expander)",
                       Test_Expand_With_Expander'Access);
   end Add_Tests;

   --  ------------------------------
   --  Check if the string is a reserved keyword.
   --  ------------------------------
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean is
      pragma Unreferenced (D, Name);
   begin
      return False;
   end Is_Reserved;

   --  ------------------------------
   --  Test expand SQL with parameters.
   --  ------------------------------
   procedure Test_Expand_Sql (T : in out Test) is
      SQL : ADO.Parameters.List;
      D   : aliased Dialect;

      procedure Check (Pattern : in String; Expect : in String);

      procedure Check (Pattern : in String; Expect : in String) is
         Result : constant String := SQL.Expand (Pattern);
      begin
         Assert_Equals (T, Expect, Result, "Invalid SQL expansion");
      end Check;

   begin
      SQL.Set_Dialect (D'Unchecked_Access);
      SQL.Bind_Param (1, "select '");
      SQL.Bind_Param (2, "from");
      SQL.Bind_Param ("user_id", String '("23"));
      SQL.Bind_Param ("object_23_identifier", Integer (44));
      SQL.Bind_Param ("bool", True);
      SQL.Bind_Param (6, False);
      SQL.Bind_Param ("_date", Ada.Calendar.Clock);
      SQL.Bind_Null_Param ("_null");

      Check ("?", "'select '''");
      Check (":2", "'from'");
      Check (":6", "0");
      Check (":user_id", "'23'");
      Check (":bool", "1");
      Check (":_null", "NULL");

      Check ("select :1 :2 :3 :4 :5 :6", "select 'select ''' 'from' '23' 44 1 0");
      Check ("select ? ? ? ? ? ?", "select 'select ''' 'from' '23' 44 1 0");
      Check ("select ? :2 :user_id :object_23_identifier :bool :6",
             "select 'select ''' 'from' '23' 44 1 0");
      Check ("select \:1 \:2 \:3 \:4 \:5 \:6", "select :1 :2 :3 :4 :5 :6");
   end Test_Expand_Sql;

   --  ------------------------------
   --  Test expand with invalid parameters.
   --  ------------------------------
   procedure Test_Expand_Error (T : in out Test) is
      SQL : ADO.Parameters.List;

      procedure Check (Pattern : in String; Expect : in String);

      procedure Check (Pattern : in String; Expect : in String) is
         Result : constant String := SQL.Expand (Pattern);
      begin
         Assert_Equals (T, Expect, Result, "Invalid SQL expansion");
      end Check;

   begin
      Check (":<", "<");
      Check (":234", "");
      Check (":name", "");
      Check ("select :", "select :");
   end Test_Expand_Error;

   --  ------------------------------
   --  Test expand performance.
   --  ------------------------------
   procedure Test_Expand_Perf (T : in out Test) is
      pragma Unreferenced (T);

      SQL : ADO.Parameters.List;
      D   : aliased Dialect;
   begin
      SQL.Set_Dialect (D'Unchecked_Access);
      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            SQL.Bind_Param (I, I);
         end loop;
         Util.Measures.Report (T, "Bind_Param (1000 calls)");
      end;

      declare
         B : constant Unbounded_String
           := To_Unbounded_String ("select t.a, t.b, t.c, t.d, t.e, t.f "
                                   & "from T where t.b = 23");
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               S : constant String := To_String (B);
               pragma Unreferenced (S);
            begin
               null;
            end;
         end loop;
         Util.Measures.Report (T, "Expand reference To_String");
      end;
      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               S : constant String := SQL.Expand ("select t.a, t.b, t.c, t.d, t.e, t.f "
                                                  & "from T where t.b = 23");
               pragma Unreferenced (S);
            begin
               null;
            end;
         end loop;
         Util.Measures.Report (T, "Expand (1000 calls with 0 parameter)");
      end;
      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               S : constant String := SQL.Expand ("select t.a, t.b, t.c, t.d, t.e, t.f "
                                                  & "from T where t.b = :10");
               pragma Unreferenced (S);
            begin
               null;
            end;
         end loop;
         Util.Measures.Report (T, "Expand (1000 calls with 1 parameter)");
      end;
      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               S : constant String := SQL.Expand (":10 :20 :30 :40 :50 :60 :70 :80 :90 :100");
               pragma Unreferenced (S);
            begin
               null;
            end;
         end loop;
         Util.Measures.Report (T, "Expand (1000 calls with 10 parameters)");
      end;
   end Test_Expand_Perf;

   --  ------------------------------
   --  Test expand with cache expander.
   --  ------------------------------
   procedure Test_Expand_With_Expander (T : in out Test) is
      procedure Check (Content : in String;
                       Expect  : in String);

      SQL : ADO.Parameters.List;
      D   : aliased Dialect;
      C   : aliased ADO.Caches.Cache_Manager;
      M   : access Int_Cache.Cache_Type := new Int_Cache.Cache_Type;
      P   : access Int_Cache.Cache_Type := new Int_Cache.Cache_Type;

      procedure Check (Content : in String;
                       Expect  : in String) is
         S : constant String := SQL.Expand (Content);
      begin
         Util.Tests.Assert_Equals (T, Expect, S, "Invalid expand for SQL '" & Content & "'");
      end Check;

   begin
      C.Add_Cache ("test-group", M.all'Access);
      C.Add_Cache ("G2", P.all'Access);
      SQL.Expander := C'Unchecked_Access;
      M.Insert ("value-1", 123);
      M.Insert ("2", 2);
      M.Insert ("a name", 10);
      P.Insert ("1", 10);
      P.Insert ("a", 0);
      P.Insert ("c", 1);
      SQL.Set_Dialect (D'Unchecked_Access);
      Check ("$test-group[2]", "2");
      Check ("$test-group[a name]", "10");
      Check ("SELECT * FROM user WHERE id = $test-group[2]+$test-group[value-1]",
             "SELECT * FROM user WHERE id = 2+123");
      Check ("SELECT * FROM user WHERE id = $G2[a]+$test-group[value-1]",
             "SELECT * FROM user WHERE id = 0+123");
      P.Insert ("a", 1, True);
      Check ("SELECT * FROM user WHERE id = $G2[a]+$test-group[value-1]",
             "SELECT * FROM user WHERE id = 1+123");
      Check ("SELECT * FROM user WHERE id = 2$G2[titi]+$test[value-1]1",
             "SELECT * FROM user WHERE id = 2+1");
   end Test_Expand_With_Expander;

end ADO.Parameters.Tests;
