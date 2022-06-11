-----------------------------------------------------------------------
--  Regtests.Audits.Model -- Regtests.Audits.Model
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.2.3
-----------------------------------------------------------------------
--  Copyright (C) 2022 Stephane Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
with ADO.Audits;
pragma Warnings (On);
package Regtests.Audits.Model is

   pragma Style_Checks ("-mr");

   type Audit_Ref is new ADO.Objects.Object_Ref with null record;

   type Email_Ref is new ADO.Objects.Object_Ref with null record;

   type Property_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  This is the Audit_Info table
   --  --------------------
   --  Create an object key for Audit.
   function Audit_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Audit from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Audit_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Audit : constant Audit_Ref;
   function "=" (Left, Right : Audit_Ref'Class) return Boolean;

   --  Set null
   procedure Set_Id (Object : in out Audit_Ref;
                     Value  : in ADO.Identifier);

   --  Get null
   function Get_Id (Object : in Audit_Ref)
                 return ADO.Identifier;

   --  Set the entity id
   procedure Set_Entity_Id (Object : in out Audit_Ref;
                            Value  : in ADO.Identifier);

   --  Get the entity id
   function Get_Entity_Id (Object : in Audit_Ref)
                 return ADO.Identifier;

   --  Set the entity type
   procedure Set_Entity_Type (Object : in out Audit_Ref;
                              Value  : in ADO.Entity_Type);

   --  Get the entity type
   function Get_Entity_Type (Object : in Audit_Ref)
                 return ADO.Entity_Type;

   --  Set the old value
   procedure Set_Old_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String);
   procedure Set_Old_Value (Object : in out Audit_Ref;
                            Value : in String);

   --  Get the old value
   function Get_Old_Value (Object : in Audit_Ref)
                 return ADO.Nullable_String;
   function Get_Old_Value (Object : in Audit_Ref)
                 return String;

   --  Set the new value
   procedure Set_New_Value (Object : in out Audit_Ref;
                            Value  : in ADO.Nullable_String);
   procedure Set_New_Value (Object : in out Audit_Ref;
                            Value : in String);

   --  Get the new value
   function Get_New_Value (Object : in Audit_Ref)
                 return ADO.Nullable_String;
   function Get_New_Value (Object : in Audit_Ref)
                 return String;

   --  Set the audit date
   procedure Set_Date (Object : in out Audit_Ref;
                       Value  : in Ada.Calendar.Time);

   --  Get the audit date
   function Get_Date (Object : in Audit_Ref)
                 return Ada.Calendar.Time;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Audit_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Audit_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Audit_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   AUDIT_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Audit_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Audit_Ref;
                   Into   : in out Audit_Ref);

   package Audit_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Audit_Ref,
                                  "="          => "=");
   subtype Audit_Vector is Audit_Vectors.Vector;

   procedure List (Object  : in out Audit_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);
   --  --------------------
   --  This is the User email table
   --  --------------------
   --  Create an object key for Email.
   function Email_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Email from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Email_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Email : constant Email_Ref;
   function "=" (Left, Right : Email_Ref'Class) return Boolean;

   --  Set null
   procedure Set_Id (Object : in out Email_Ref;
                     Value  : in ADO.Identifier);

   --  Get null
   function Get_Id (Object : in Email_Ref)
                 return ADO.Identifier;

   --  Set the user email address
   procedure Set_Email (Object : in out Email_Ref;
                        Value  : in ADO.Nullable_String);
   procedure Set_Email (Object : in out Email_Ref;
                        Value : in String);

   --  Get the user email address
   function Get_Email (Object : in Email_Ref)
                 return ADO.Nullable_String;
   function Get_Email (Object : in Email_Ref)
                 return String;

   --  Set the user email status
   procedure Set_Status (Object : in out Email_Ref;
                         Value  : in ADO.Nullable_Integer);

   --  Get the user email status
   function Get_Status (Object : in Email_Ref)
                 return ADO.Nullable_Integer;

   --  Set the email date
   procedure Set_Date (Object : in out Email_Ref;
                       Value  : in ADO.Nullable_Time);

   --  Get the email date
   function Get_Date (Object : in Email_Ref)
                 return ADO.Nullable_Time;

   --  Set the email creation date
   procedure Set_Create_Date (Object : in out Email_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the email creation date
   function Get_Create_Date (Object : in Email_Ref)
                 return Ada.Calendar.Time;

   --  Set the email info
   procedure Set_Info (Object : in out Email_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Info (Object : in out Email_Ref;
                       Value : in String);

   --  Get the email info
   function Get_Info (Object : in Email_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Info (Object : in Email_Ref)
                 return String;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Email_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Email_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Email_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   EMAIL_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Email_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Email_Ref;
                   Into   : in out Email_Ref);

   --  --------------------
   --  This is a generic property
   --  --------------------
   --  Create an object key for Property.
   function Property_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Property from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Property_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Property : constant Property_Ref;
   function "=" (Left, Right : Property_Ref'Class) return Boolean;

   --  Set null
   procedure Set_Id (Object : in out Property_Ref;
                     Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Id (Object : in out Property_Ref;
                     Value : in String);

   --  Get null
   function Get_Id (Object : in Property_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Id (Object : in Property_Ref)
                 return String;

   --  Set the property value
   procedure Set_Value (Object : in out Property_Ref;
                        Value  : in ADO.Nullable_Integer);

   --  Get the property value
   function Get_Value (Object : in Property_Ref)
                 return ADO.Nullable_Integer;

   --  Set a float property value
   procedure Set_Float_Value (Object : in out Property_Ref;
                              Value  : in Float);

   --  Get a float property value
   function Get_Float_Value (Object : in Property_Ref)
                 return Float;

   --  Set a double property value
   procedure Set_Double_Value (Object : in out Property_Ref;
                               Value  : in Long_Float);

   --  Get a double property value
   function Get_Double_Value (Object : in Property_Ref)
                 return Long_Float;

   --  Set the property entity type
   procedure Set_Kind (Object : in out Property_Ref;
                       Value  : in ADO.Entity_Type);

   --  Get the property entity type
   function Get_Kind (Object : in Property_Ref)
                 return ADO.Entity_Type;

   --  Set the optional property entity type
   procedure Set_Optional_Kind (Object : in out Property_Ref;
                                Value  : in ADO.Nullable_Entity_Type);

   --  Get the optional property entity type
   function Get_Optional_Kind (Object : in Property_Ref)
                 return ADO.Nullable_Entity_Type;

   --  Set the optional object_id
   procedure Set_Object_Id (Object : in out Property_Ref;
                            Value  : in ADO.Identifier);

   --  Get the optional object_id
   function Get_Object_Id (Object : in Property_Ref)
                 return ADO.Identifier;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Property_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Ada.Strings.Unbounded.Unbounded_String);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Property_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in Ada.Strings.Unbounded.Unbounded_String;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Property_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Property_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Property_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Property_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   PROPERTY_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Property_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Property_Ref;
                   Into   : in out Property_Ref);




private
   AUDIT_NAME : aliased constant String := "audit_info";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "entity_id";
   COL_2_1_NAME : aliased constant String := "entity_type";
   COL_3_1_NAME : aliased constant String := "old_value";
   COL_4_1_NAME : aliased constant String := "new_value";
   COL_5_1_NAME : aliased constant String := "date";

   AUDIT_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 6,
      Table   => AUDIT_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access)
     );
   AUDIT_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := AUDIT_DEF'Access;


   Null_Audit : constant Audit_Ref
      := Audit_Ref'(ADO.Objects.Object_Ref with null record);

   type Audit_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => AUDIT_DEF'Access)
   with record
       Entity_Id : ADO.Identifier;
       Entity_Type : ADO.Entity_Type;
       Old_Value : ADO.Nullable_String;
       New_Value : ADO.Nullable_String;
       Date : Ada.Calendar.Time;
   end record;

   type Audit_Access is access all Audit_Impl;

   overriding
   procedure Destroy (Object : access Audit_Impl);

   overriding
   procedure Find (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Audit_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Audit_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Audit_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Audit_Ref'Class;
                        Impl   : out Audit_Access);
   EMAIL_NAME : aliased constant String := "audit_email";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "user_email";
   COL_2_2_NAME : aliased constant String := "email_status";
   COL_3_2_NAME : aliased constant String := "email_date";
   COL_4_2_NAME : aliased constant String := "email_create_date";
   COL_5_2_NAME : aliased constant String := "email_info";

   EMAIL_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 6,
      Table   => EMAIL_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access,
         6 => COL_5_2_NAME'Access)
     );
   EMAIL_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := EMAIL_DEF'Access;

   EMAIL_AUDIT_DEF : aliased constant ADO.Audits.Auditable_Mapping :=
     (Count    => 5,
      Of_Class => EMAIL_DEF'Access,
      Members  => (
         1 => 1,
         2 => 2,
         3 => 3,
         4 => 4,
         5 => 5)
     );
   EMAIL_AUDIT_TABLE : constant ADO.Audits.Auditable_Mapping_Access
      := EMAIL_AUDIT_DEF'Access;

   Null_Email : constant Email_Ref
      := Email_Ref'(ADO.Objects.Object_Ref with null record);

   type Email_Impl is
      new ADO.Audits.Auditable_Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => EMAIL_DEF'Access,
                                     With_Audit => EMAIL_AUDIT_DEF'Access)
   with record
       Email : ADO.Nullable_String;
       Status : ADO.Nullable_Integer;
       Date : ADO.Nullable_Time;
       Create_Date : Ada.Calendar.Time;
       Info : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Email_Access is access all Email_Impl;

   overriding
   procedure Destroy (Object : access Email_Impl);

   overriding
   procedure Find (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Email_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Email_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Email_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Email_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Email_Ref'Class;
                        Impl   : out Email_Access);
   PROPERTY_NAME : aliased constant String := "audit_property";
   COL_0_3_NAME : aliased constant String := "id";
   COL_1_3_NAME : aliased constant String := "user_email";
   COL_2_3_NAME : aliased constant String := "float_value";
   COL_3_3_NAME : aliased constant String := "double_value";
   COL_4_3_NAME : aliased constant String := "kind";
   COL_5_3_NAME : aliased constant String := "optional_kind";
   COL_6_3_NAME : aliased constant String := "object_id";

   PROPERTY_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 7,
      Table   => PROPERTY_NAME'Access,
      Members => (
         1 => COL_0_3_NAME'Access,
         2 => COL_1_3_NAME'Access,
         3 => COL_2_3_NAME'Access,
         4 => COL_3_3_NAME'Access,
         5 => COL_4_3_NAME'Access,
         6 => COL_5_3_NAME'Access,
         7 => COL_6_3_NAME'Access)
     );
   PROPERTY_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := PROPERTY_DEF'Access;

   PROPERTY_AUDIT_DEF : aliased constant ADO.Audits.Auditable_Mapping :=
     (Count    => 6,
      Of_Class => PROPERTY_DEF'Access,
      Members  => (
         1 => 1,
         2 => 2,
         3 => 3,
         4 => 4,
         5 => 5,
         6 => 6)
     );
   PROPERTY_AUDIT_TABLE : constant ADO.Audits.Auditable_Mapping_Access
      := PROPERTY_AUDIT_DEF'Access;

   Null_Property : constant Property_Ref
      := Property_Ref'(ADO.Objects.Object_Ref with null record);

   type Property_Impl is
      new ADO.Audits.Auditable_Object_Record (Key_Type => ADO.Objects.KEY_STRING,
                                     Of_Class => PROPERTY_DEF'Access,
                                     With_Audit => PROPERTY_AUDIT_DEF'Access)
   with record
       Value : ADO.Nullable_Integer;
       Float_Value : Float;
       Double_Value : Long_Float;
       Kind : ADO.Entity_Type;
       Optional_Kind : ADO.Nullable_Entity_Type;
       Object_Id : ADO.Identifier;
   end record;

   type Property_Access is access all Property_Impl;

   overriding
   procedure Destroy (Object : access Property_Impl);

   overriding
   procedure Find (Object  : in out Property_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Property_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Property_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Property_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Property_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Property_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Property_Ref'Class;
                        Impl   : out Property_Access);
end Regtests.Audits.Model;
