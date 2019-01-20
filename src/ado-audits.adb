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
with Util.Beans.Objects.Time;
with ADO.Utils;
package body ADO.Audits is

   use Ada.Strings.Unbounded;
   use type ADO.Schemas.Column_Index;
   use type Ada.Calendar.Time;

   package UBOT renames Util.Beans.Objects.Time;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Audit_Array,
                                     Name   => Audit_Array_Access);

   procedure Save (Object  : in out Auditable_Object_Record'Class;
                   Session : in out ADO.Sessions.Master_Session'Class) is
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

   procedure Set_Field_Unbounded_String (Object : in out Auditable_Object_Record'Class;
                                         Field  : in Column_Index;
                                         Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                                         Value  : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Unbounded_String;

   procedure Set_Field_String (Object : in out Auditable_Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                               Value  : in String) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Ada.Strings.Unbounded.Set_Unbounded_String (Into, Value);
         Object.Set_Field (Field);
      end if;
   end Set_Field_String;

   procedure Set_Field_String (Object : in out Auditable_Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out ADO.Nullable_String;
                               Value  : in String) is
   begin
      if Into.Is_Null or else Into.Value /= Value then
         if Into.Is_Null then
            Object.Audit_Field (Field, UBO.Null_Object, UBO.To_Object (Value));
         else
            Object.Audit_Field (Field, UBO.To_Object (Into.Value), UBO.To_Object (Value));
         end if;
         Into.Is_Null := False;
         Ada.Strings.Unbounded.Set_Unbounded_String (Into.Value, Value);
         Object.Set_Field (Field);
      end if;
   end Set_Field_String;

   procedure Set_Field_String (Object : in out Auditable_Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out ADO.Nullable_String;
                               Value  : in ADO.Nullable_String) is
   begin
      if Into.Is_Null then
         if not Value.Is_Null then
            Object.Audit_Field (Field, UBO.Null_Object, UBO.To_Object (Value.Value));
            Into := Value;
            Object.Set_Field (Field);
         end if;
      elsif Value.Is_Null then
         Object.Audit_Field (Field, UBO.To_Object (Into.Value), UBO.Null_Object);
         Into := Value;
         Object.Set_Field (Field);
      elsif Into.Value /= Value.Value then
         Object.Audit_Field (Field, UBO.To_Object (Into.Value), UBO.To_Object (Value.Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_String;

   procedure Set_Field_Time (Object : in out Auditable_Object_Record'Class;
                             Field  : in Column_Index;
                             Into   : in out Ada.Calendar.Time;
                             Value  : in Ada.Calendar.Time) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBOT.To_Object (Into), UBOT.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Time;

   procedure Set_Field_Time (Object : in out Auditable_Object_Record'Class;
                             Field  : in Column_Index;
                             Into   : in out ADO.Nullable_Time;
                             Value  : in ADO.Nullable_Time) is
   begin
      if Into.Is_Null then
         if not Value.Is_Null then
            Object.Audit_Field (Field, UBO.Null_Object, UBOT.To_Object (Value.Value));
            Into := Value;
            Object.Set_Field (Field);
         end if;
      elsif Value.Is_Null then
         Object.Audit_Field (Field, UBOT.To_Object (Into.Value), UBO.Null_Object);
         Into := Value;
         Object.Set_Field (Field);
      elsif Into.Value /= Value.Value then
         Object.Audit_Field (Field, UBOT.To_Object (Into.Value), UBOT.To_Object (Value.Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Time;

   procedure Set_Field_Integer (Object : in out Auditable_Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Integer;
                                Value  : in Integer) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Integer;

   procedure Set_Field_Integer (Object : in out Auditable_Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out ADO.Nullable_Integer;
                                Value  : in ADO.Nullable_Integer) is
   begin
      if Into.Is_Null then
         if not Value.Is_Null then
            Object.Audit_Field (Field, UBO.Null_Object, UBO.To_Object (Value.Value));
            Into := Value;
            Object.Set_Field (Field);
         end if;
      elsif Value.Is_Null then
         Object.Audit_Field (Field, UBO.To_Object (Into.Value), UBO.Null_Object);
         Into := Value;
         Object.Set_Field (Field);
      elsif Into.Value /= Value.Value then
         Object.Audit_Field (Field, UBO.To_Object (Into.Value), UBO.To_Object (Value.Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Integer;

   procedure Set_Field_Natural (Object : in out Auditable_Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Natural;
                                Value  : in Natural) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Natural;

   procedure Set_Field_Positive (Object : in out Auditable_Object_Record'Class;
                                 Field  : in Column_Index;
                                 Into   : in out Positive;
                                 Value  : in Positive) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Positive;

   procedure Set_Field_Boolean (Object : in out Auditable_Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Boolean;
                                Value  : in Boolean) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Boolean;

   procedure Set_Field_Float (Object : in out Auditable_Object_Record'Class;
                              Field  : in Column_Index;
                              Into   : in out Float;
                              Value  : in Float) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Float;

   procedure Set_Field_Long_Float (Object : in out Auditable_Object_Record'Class;
                                   Field  : in Column_Index;
                                   Into   : in out Long_Float;
                                   Value  : in Long_Float) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Into), UBO.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Long_Float;

   procedure Set_Field_Object (Object : in out Auditable_Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out ADO.Objects.Object_Ref'Class;
                               Value  : in ADO.Objects.Object_Ref'Class) is
      use type ADO.Objects.Object_Ref;
   begin
      if Into /= Value then
         Object.Audit_Field (Field, ADO.Objects.To_Object (Into.Get_Key),
                             ADO.Objects.To_Object (Value.Get_Key));
         ADO.Objects.Set_Field_Object (Object, Field, Into, Value);
      end if;
   end Set_Field_Object;

   procedure Set_Field_Identifier (Object : in out Auditable_Object_Record'Class;
                                   Field  : in Column_Index;
                                   Into   : in out ADO.Identifier;
                                   Value  : in ADO.Identifier) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, Utils.To_Object (Into), Utils.To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Identifier;

   procedure Set_Field_Entity_Type (Object : in out Auditable_Object_Record'Class;
                                    Field  : in Column_Index;
                                    Into   : in out ADO.Entity_Type;
                                    Value  : in ADO.Entity_Type) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, UBO.To_Object (Natural (Into)),
                             UBO.To_Object (Natural (Value)));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Entity_Type;

   procedure Set_Field_Entity_Type (Object : in out Auditable_Object_Record'Class;
                                    Field  : in Column_Index;
                                    Into   : in out ADO.Nullable_Entity_Type;
                                    Value  : in ADO.Nullable_Entity_Type) is
   begin
      if Into.Is_Null then
         if not Value.Is_Null then
            Object.Audit_Field (Field, UBO.Null_Object, UBO.To_Object (Natural (Value.Value)));
            Into := Value;
            Object.Set_Field (Field);
         end if;
      elsif Value.Is_Null then
         Object.Audit_Field (Field, UBO.To_Object (Natural (Into.Value)), UBO.Null_Object);
         Into := Value;
         Object.Set_Field (Field);
      elsif Into.Value /= Value.Value then
         Object.Audit_Field (Field, UBO.To_Object (Natural (Into.Value)),
                             UBO.To_Object (Natural (Value.Value)));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Entity_Type;

   procedure Set_Field_Key_Value (Object : in out Auditable_Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in ADO.Identifier) is
   begin
      if Object.Get_Key_Value /= Value then
         Object.Audit_Field (Field, ADO.Objects.To_Object (Object.Get_Key),
                             Utils.To_Object (Value));
         ADO.Objects.Set_Key_Value (Object, Value);
         Object.Set_Field (Field);
      end if;
   end Set_Field_Key_Value;

   procedure Set_Field_Key_Value (Object : in out Auditable_Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in String) is
   begin
      if Object.Get_Key_Value /= Value then
         Object.Audit_Field (Field, ADO.Objects.To_Object (Object.Get_Key),
                             UBO.To_Object (Value));
         ADO.Objects.Set_Key_Value (Object, Value);
         Object.Set_Field (Field);
      end if;
   end Set_Field_Key_Value;

   procedure Set_Field_Key_Value (Object : in out Auditable_Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Object.Get_Key_Value /= Value then
         Object.Audit_Field (Field, ADO.Objects.To_Object (Object.Get_Key),
                             UBO.To_Object (Value));
         ADO.Objects.Set_Key_Value (Object, Value);
         Object.Set_Field (Field);
      end if;
   end Set_Field_Key_Value;

   procedure Set_Field_Operation (Object : in out Auditable_Object_Record'Class;
                                  Field  : in Column_Index;
                                  Into   : in out T;
                                  Value  : in T) is
   begin
      if Into /= Value then
         Object.Audit_Field (Field, To_Object (Into), To_Object (Value));
         Into := Value;
         Object.Set_Field (Field);
      end if;
   end Set_Field_Operation;

end ADO.Audits;
