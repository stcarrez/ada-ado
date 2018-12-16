-----------------------------------------------------------------------
--  ado-audits -- Auditing support
--  Copyright (C) 2018 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

package body ADO.Audits is

   use type ADO.Schemas.Column_Index;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Audit_Array,
                                     Name   => Audit_Array_Access);

   procedure Save (Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in out Auditable_Object_Record'Class) is
      Manager : constant access Audit_Manager'Class := Session.Get_Audit_Manager;
      Audits  : constant Audit_Array_Access := Object.Audits;
      Last    : Audit_Info_Index;
   begin
      if Manager /= null and Audits /= null then
         Last := 1;
         for Pos in Audits'Range loop
            exit when Audits (Pos).Field = 0;
            Last := Pos;
         end loop;
         Manager.Save (Session, Object, Audits (1 .. Last));
      end if;
      Free (Object.Audits);
   end Save;

   --  --------------------
   --  Record an audit information for a field.
   --  --------------------
   procedure Audit_Field (Object    : in out Auditable_Object_Record;
                          Field     : in ADO.Schemas.Column_Index;
                          Old_Value : in UBO.Object;
                          New_Value : in UBO.Object) is
      Audits : Audit_Array_Access := Object.Audits;
   begin
      if Audits = null then
         Audits := new Audit_Array (1 .. Audit_Info_Index (Object.With_Audit.Count));
         Object.Audits := Audits;
      end if;
      for Pos in Object.Audits'Range loop
         if Audits (Pos).Field = Field then
            Audits (Pos).New_Value := New_Value;
            return;
         end if;
         if Audits (Pos).Field = 0 then
            Audits (Pos).Field := Field;
            Audits (Pos).Old_Value := Old_Value;
            Audits (Pos).New_Value := New_Value;
            return;
         end if;
      end loop;
   end Audit_Field;

   --  --------------------
   --  Release the object.
   --  --------------------
   overriding
   procedure Finalize (Object : in out Auditable_Object_Record) is
   begin
      Free (Object.Audits);
      ADO.Objects.Finalize (ADO.Objects.Object_Record (Object));
   end Finalize;

end ADO.Audits;
