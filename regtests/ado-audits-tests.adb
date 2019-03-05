-----------------------------------------------------------------------
--  ado-audits-tests -- Audit tests
--  Copyright (C) 2018, 2019 Stephane Carrez
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

with ADO.SQL;
with ADO.Sessions.Entities;
package body ADO.Audits.Tests is

   use type ADO.Objects.Object_Key_Type;

   package Caller is new Util.Test_Caller (Test, "ADO.Audits");

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
   begin
      for I in List'Range loop
         Email := Regtests.Audits.Model.Null_Email;
         Email.Set_Email ("Email" & Util.Strings.Image (I) & "@nowhere.com");
         Email.Set_Status (ADO.Nullable_Integer '(23, False));
         Email.Save (DB);
         List (I) := Email.Get_Id;
      end loop;
      DB.Commit;

      for Id of List loop
         Email.Load (DB, Id);
         Email.Set_Status (ADO.Nullable_Integer '(Email.Get_Status.Value + 1, False));
         Email.Save (DB);

         Email.Set_Email ("Email" & Util.Strings.Image (Email.Get_Status.Value) & "@here.com");
         Email.Save (DB);

         Email.Set_Email ("Email" & Util.Strings.Image (Email.Get_Status.Value) & "@there.com");
         Email.Set_Status (ADO.Nullable_Integer '(Email.Get_Status.Value + 1, False));
         Email.Save (DB);
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
         Prop.Save (DB);
      end loop;

      declare
         Query : ADO.SQL.Query;
         Audit_List : Regtests.Audits.Model.Audit_Vector;
      begin
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

            Util.Tests.Assert_Equals (T, 7, Natural (Audit_List.Length),
                                      "Invalid number of audit records");
         end loop;
      end;
   end Test_Audit_Field;

end ADO.Audits.Tests;
