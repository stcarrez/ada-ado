-----------------------------------------------------------------------
--  ado-audits-tests -- Audit tests
--  Copyright (C) 2018, 2019, 2021, 2022 Stephane Carrez
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
with Util.Strings;

with Ada.Text_IO;
with Regtests.Audits.Model;

with ADO.Queries.Loaders;
with ADO.Datasets;
with ADO.Sessions.Entities;
package body ADO.Audits.Tests is

   use Ada.Strings.Unbounded;
   use type ADO.Objects.Object_Key_Type;

   package Caller is new Util.Test_Caller (Test, "ADO.Audits");

   package Audit_List_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/audit-list.xml",
                                   Sha1 => "");

   package Property_List_Query is
     new ADO.Queries.Loaders.Query (Name => "property-list",
                                    File => Audit_List_Query_File.File'Access);

   package Email_List_Query is
     new ADO.Queries.Loaders.Query (Name => "email-list",
                                    File => Audit_List_Query_File.File'Access);

   type Test_Audit_Manager is new Audit_Manager with null record;

   --  Save the audit changes in the database.
   overriding
   procedure Save (Manager : in out Test_Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in Auditable_Object_Record'Class;
                   Changes : in Audit_Array);

   Audit_Instance : aliased Test_Audit_Manager;

   --  Save the audit changes in the database.
   overriding
   procedure Save (Manager : in out Test_Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in Auditable_Object_Record'Class;
                   Changes : in Audit_Array) is
      pragma Unreferenced (Manager);

      Now  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Kind : constant ADO.Entity_Type
        := ADO.Sessions.Entities.Find_Entity_Type (Session, Object.Get_Key);
   begin
      for C of Changes loop
         declare
            Audit : Regtests.Audits.Model.Audit_Ref;
         begin
            if Object.Key_Type = ADO.Objects.KEY_INTEGER then
               Audit.Set_Entity_Id (ADO.Objects.Get_Value (Object.Get_Key));
            end if;
            Audit.Set_Entity_Type (Kind);
            Audit.Set_Old_Value (UBO.To_String (C.Old_Value));
            Audit.Set_New_Value (UBO.To_String (C.New_Value));
            Audit.Set_Date (Now);
            Audit.Save (Session);
         end;
      end loop;
   end Save;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Audits.Audit_Field",
                       Test_Audit_Field'Access);
   end Add_Tests;

   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Regtests.Set_Audit_Manager (Audit_Instance'Access);
   end Set_Up;

   --  ------------------------------
   --  Test populating Audit_Fields
   --  ------------------------------
   procedure Test_Audit_Field (T : in out Test) is
      type Identifier_Array is array (1 .. 10) of ADO.Identifier;

      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Email  : Regtests.Audits.Model.Email_Ref;
      Prop   : Regtests.Audits.Model.Property_Ref;
      List   : Identifier_Array;
      E1     : ADO.Entity_Type;
      E2     : ADO.Entity_Type;
      J      : ADO.Identifier := 0;
      Query  : ADO.Queries.Context;
      Prop_Count  : Natural;
      Email_Count : Natural;
   begin
      Query.Set_Count_Query (Property_List_Query.Query'Access);
      Prop_Count := ADO.Datasets.Get_Count (DB, Query);

      Query.Set_Count_Query (Email_List_Query.Query'Access);
      Email_Count := ADO.Datasets.Get_Count (DB, Query);

      E1 := ADO.Sessions.Entities.Find_Entity_Type (DB, "audit_email");
      E2 := ADO.Sessions.Entities.Find_Entity_Type (DB, "audit_property");
      for I in List'Range loop
         Email := Regtests.Audits.Model.Null_Email;
         Email.Set_Email ("Email" & Util.Strings.Image (I) & "@nowhere.com");
         Email.Set_Status (ADO.Nullable_Integer '(23, False));
         Email.Set_Create_Date (Ada.Calendar.Clock);
         Email.Set_Info ("Info");
         Email.Save (DB);
         List (I) := Email.Get_Id;
      end loop;
      DB.Commit;

      for Id of List loop
         Email.Load (DB, Id);
         Email.Set_Status (ADO.Nullable_Integer '(Email.Get_Status.Value + 1, False));
         Email.Set_Date (ADO.Nullable_Time '(Ada.Calendar.Clock, False));
         Email.Save (DB);

         Email.Set_Email ("Email" & Util.Strings.Image (Natural (Id))
                            & Util.Strings.Image (Email.Get_Status.Value) & "@here.com");
         Email.Save (DB);

         --  Set a null string, null integer.
         Email.Set_Email (ADO.Null_String);
         Email.Set_Status (ADO.Null_Integer);
         Email.Set_Date (ADO.Null_Time);
         Email.Save (DB);

         Email.Set_Email ("Email" & Util.Strings.Image (Natural (Id))
                            & Util.Strings.Image (Email.Get_Status.Value) & "@there.com");
         Email.Set_Status (ADO.Nullable_Integer '(Email.Get_Status.Value + 1, False));
         Email.Set_Date (ADO.Nullable_Time '(Ada.Calendar.Clock, False));
         Email.Save (DB);

         declare
            New_Email : constant Unbounded_String
              := To_Unbounded_String ("Email" & Util.Strings.Image (Natural (Id))
                                      & Util.Strings.Image (Email.Get_Status.Value)
                                      & "@there.com");
         begin
            Email.Set_Info (New_Email);
            Email.Set_Email (ADO.Nullable_String '(New_Email, False));
            Email.Set_Status (ADO.Nullable_Integer '(24, False));
            Email.Set_Date (ADO.Nullable_Time '(Ada.Calendar.Clock, False));
            Email.Save (DB);
         end;

         --  Set a null string.
         Email.Set_Email (ADO.Null_String);
         Email.Save (DB);

         declare
            New_Email : constant Unbounded_String
              := To_Unbounded_String ("Email-3-" & Util.Strings.Image (Natural (Id))
                                      & Util.Strings.Image (Email.Get_Status.Value)
                                      & "@there.com");
         begin
            Email.Set_Info (New_Email);
            Email.Set_Email (ADO.Nullable_String '(New_Email, False));
            Email.Set_Status (ADO.Nullable_Integer '(25, False));
            Email.Set_Date (ADO.Nullable_Time '(Ada.Calendar.Clock, False));
            Email.Save (DB);
         end;

      end loop;
      DB.Commit;

      for Id of List loop
         Email.Load (DB, Id);
         Email.Set_Status (ADO.Null_Integer);
         Email.Save (DB);
      end loop;
      DB.Commit;

      Prop.set_Id (Util.Tests.Get_Uuid);
      for I in 1 .. 10 loop
         Prop.Set_Value ((Value => I, Is_Null => False));
         Prop.Set_Float_Value (3.0 * Float (I));
         Prop.Set_Double_Value (123.0 * Long_Float (I));
         Prop.Set_Kind ((if I mod 2 = 0 then E1 else E2));
         if I mod 4 = 0 then
            Prop.Set_Optional_Kind (ADO.Null_Entity_Type);
         elsif I mod 2 = 0 then
            Prop.Set_Optional_Kind (ADO.Nullable_Entity_Type '(E1, False));
         else
            Prop.Set_Optional_Kind (ADO.Nullable_Entity_Type '(E2, False));
         end if;
         Prop.Set_Object_Id (J);
         Prop.Save (DB);
         J := J + 1;
      end loop;

      declare
         Audit_List : Regtests.Audits.Model.Audit_Vector;
      begin
         Query.Clear;
         Query.Set_Filter ("entity_id = :entity_id");
         for Id of List loop
            Query.Bind_Param ("entity_id", Id);
            Regtests.Audits.Model.List (Audit_List, DB, Query);
            for A of Audit_List loop
               Ada.Text_IO.Put_Line (ADO.Identifier'Image (Id) & " "
                                     & ADO.Identifier'Image (A.Get_Id)
                                     & " " & A.Get_Old_Value & " - "
                                     & A.Get_New_Value);

               Util.Tests.Assert_Equals (T, Natural (Id), Natural (A.Get_Entity_Id),
                                         "Invalid audit record: id is wrong");
            end loop;

            Util.Tests.Assert_Equals (T, 23, Natural (Audit_List.Length),
                                      "Invalid number of audit records");
         end loop;
      end;

      --  Load the properties as datasets.
      declare
         Data   : ADO.Datasets.Dataset;
      begin
         Query.Clear;
         Query.Set_Query (Property_List_Query.Query'Access);
         ADO.Datasets.List (Data, DB, Query);
         Util.Tests.Assert_Equals (T, Prop_Count + 1, Data.Get_Count,
                                   "Invalid dataset size (property)");
      end;

      --  Load the properties as datasets.
      declare
         Data   : ADO.Datasets.Dataset;
      begin
         Query.Clear;
         Query.Set_Query (Email_List_Query.Query'Access);
         ADO.Datasets.List (Data, DB, Query);
         Util.Tests.Assert_Equals (T, Email_Count + 10, Data.Get_Count,
                                   "Invalid dataset size (email)");
      end;
   end Test_Audit_Field;

end ADO.Audits.Tests;
