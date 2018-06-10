-----------------------------------------------------------------------
--  ado-sequences-tests -- Test sequences factories
--  Copyright (C) 2011, 2012, 2015, 2017, 2018 Stephane Carrez
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
with ADO.SQL;
with Regtests.Simple.Model;

with ADO.Sequences.Hilo;
with ADO.Sessions.Sources;
with ADO.Sessions.Factory;
package body ADO.Sequences.Tests is

   type Test_Impl is
     new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                    Of_Class => Regtests.Simple.Model.ALLOCATE_TABLE)
   with record
      Version : Integer;
      Value   : ADO.Identifier;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Select_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Destroy (Object : access Test_Impl);

   overriding
   procedure Find (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Test_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Test_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   package Caller is new Util.Test_Caller (Test, "ADO.Sequences");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Sequences.Create",
                       Test_Create_Factory'Access);
   end Add_Tests;

   --  Test creation of the sequence factory.
   --  This test revealed a memory leak if we failed to create a database connection.
   procedure Test_Create_Factory (T : in out Test) is
      Seq_Factory : ADO.Sequences.Factory;
      Obj         : Test_Impl;
      Factory     : aliased ADO.Sessions.Factory.Session_Factory;
      Controller  : aliased ADO.Sessions.Sources.Data_Source;
      Prev_Id     : Identifier := ADO.NO_IDENTIFIER;
   begin
      Seq_Factory.Set_Default_Generator (ADO.Sequences.Hilo.Create_HiLo_Generator'Access,
                                         Factory'Unchecked_Access);
      begin
         Seq_Factory.Allocate (Obj);
         T.Assert (False, "No exception raised.");

      exception
         when ADO.Drivers.Connection_Error =>
            null;  --  Good! An exception is expected because the session factory is empty.
      end;

      --  Make a real connection.
      Controller.Set_Connection (ADO.Drivers.Get_Config ("test.database"));
      Factory.Create (Controller);
      for I in 1 .. 1_000 loop
         Seq_Factory.Allocate (Obj);
         T.Assert (Obj.Get_Key_Value /= Prev_Id, "Invalid id was allocated");
         Prev_Id := Obj.Get_Key_Value;
      end loop;
   end Test_Create_Factory;

   overriding
   procedure Destroy (Object : access Test_Impl) is
   begin
      null;
   end Destroy;

   overriding
   procedure Find (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is
      pragma Unreferenced (Object, Session, Query);
   begin
      Found := False;
   end Find;

   overriding
   procedure Load (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Session'Class) is
   begin
      null;
   end Load;

   overriding
   procedure Save (Object  : in out Test_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class) is
   begin
      null;
   end Save;

   overriding
   procedure Delete (Object  : in out Test_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
   begin
      null;
   end Delete;

   overriding
   procedure Create (Object  : in out Test_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class) is
   begin
      null;
   end Create;

end ADO.Sequences.Tests;
