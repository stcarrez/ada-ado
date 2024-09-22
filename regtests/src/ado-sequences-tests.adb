-----------------------------------------------------------------------
--  ado-sequences-tests -- Test sequences factories
--  Copyright (C) 2011, 2012, 2015, 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;

with ADO.Configs;
with ADO.Sessions;
with ADO.SQL;
with ADO.Statements;
with Regtests.Simple.Model;

with ADO.Sequences.Hilo;
with ADO.Sessions.Sources;
with ADO.Sessions.Factory;
with ADO.Schemas;
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

   SEQUENCE_NAME : aliased constant String := "ado_sequence";

   Sequence_Table : aliased ADO.Schemas.Class_Mapping
     := ADO.Schemas.Class_Mapping '(Count => 0, Table => SEQUENCE_NAME'Access, Members => <>);

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
                                         Factory'Unchecked_Access,
                                         False);
      declare
         S : ADO.Sessions.Master_Session;
      begin
         Seq_Factory.Allocate (S, Obj);
         T.Assert (False, "No exception raised.");

      exception
         when ADO.Sessions.Connection_Error | Sessions.Session_Error =>
            null;  --  Good! An exception is expected because the session factory is empty.
      end;

      --  Make a real connection.
      Controller.Set_Connection (ADO.Configs.Get_Config ("test.database"));
      Factory.Create (Controller);
      declare
         S : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      begin
         for I in 1 .. 1_000 loop
            Seq_Factory.Allocate (S, Obj);
            T.Assert (Obj.Get_Key_Value /= Prev_Id, "Invalid id was allocated");
            Prev_Id := Obj.Get_Key_Value;
         end loop;
      end;

      --  Erase the sequence entry used for the allocate entity table.
      declare
         S : constant ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
         D : ADO.Statements.Delete_Statement := S.Create_Statement (Sequence_Table'Access);
      begin
         D.Set_Filter ("name = :name");
         D.Bind_Param ("name", String '("allocate"));
         D.Execute;

         --  Also delete all allocate items.
         D := S.Create_Statement (Regtests.Simple.Model.ALLOCATE_TABLE);
         D.Execute;
      end;

      --  Create new objects.  This forces the creation of a new entry in the sequence table.
      declare
         S : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      begin
         for I in 1 .. 1_00 loop
            Seq_Factory.Allocate (S, Obj);
            T.Assert (Obj.Get_Key_Value /= Prev_Id, "Invalid id was allocated");
            Prev_Id := Obj.Get_Key_Value;
         end loop;
      end;
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
