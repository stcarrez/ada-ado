-----------------------------------------------------------------------
--  ado-parameters-tests -- Test query parameters and SQL expansion
--  Copyright (C) 2011 Stephane Carrez
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
with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Measures;
with ADO.Sessions;
with ADO.Databases;
with ADO.Queries.Loaders;
with Regtests;
package body ADO.Parameters.Tests is

   use Util.Tests;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ADO.Parameters.Tests");

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand",
                       Test_Expand_Sql'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand (invalid params)",
                       Test_Expand_Error'Access);
      Caller.Add_Test (Suite, "Test ADO.Parameters.Expand (perf)",
                       Test_Expand_Perf'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test expand SQL with parameters.
   --  ------------------------------
   procedure Test_Expand_Sql (T : in out Test) is
      SQL : ADO.Parameters.List;

      procedure Check (Pattern : in String; Expect : in String) is
         Result : constant String := SQL.Expand (Pattern);
      begin
         Assert_Equals (T, Expect, Result, "Invalid SQL expansion");
      end Check;

   begin
      SQL.Bind_Param (1, "select '");
      SQL.Bind_Param (2, "from");
      SQL.Bind_Param ("user_id", "23");
      SQL.Bind_Param ("object_23_identifier", Integer (44));
      SQL.Bind_Param ("bool", True);
      SQL.Bind_Param (6, False);
      SQL.Bind_Param ("_date", Ada.Calendar.Clock);
      SQL.Bind_Null_Param ("_null");

      Check ("?", "'select \''");
      Check (":2", "'from'");
      Check (":6", "0");
      Check (":user_id", "'23'");
      Check (":bool", "1");
      Check (":_null", "NULL");

      Check ("select :1 :2 :3 :4 :5 :6", "select 'select \'' 'from' '23' 44 1 0");
      Check ("select ? ? ? ? ? ?", "select 'select \'' 'from' '23' 44 1 0");
      Check ("select ? :2 :user_id :object_23_identifier :bool :6",
             "select 'select \'' 'from' '23' 44 1 0");
   end Test_Expand_Sql;

   --  ------------------------------
   --  Test expand with invalid parameters.
   --  ------------------------------
   procedure Test_Expand_Error (T : in out Test) is
      SQL : ADO.Parameters.List;

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
      SQL : ADO.Parameters.List;
   begin

      declare
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            SQL.Bind_Param (I, I);
         end loop;
         Util.Measures.Report (T, "Bind_Param (1000 calls)");
      end;

      declare
         B : Unbounded_String := To_Unbounded_String ("select t.a, t.b, t.c, t.d, t.e, t.f "
                                                      & "from T where t.b = 23");
         T : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               S : constant String := To_String (B);
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
            begin
               null;
            end;
         end loop;
         Util.Measures.Report (T, "Expand (1000 calls with 10 parameters)");
      end;
   end Test_Expand_Perf;

end ADO.Parameters.Tests;
