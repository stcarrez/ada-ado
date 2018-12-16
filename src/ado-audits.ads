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

with Util.Beans.Objects;

with ADO.Configs;
with ADO.Objects;
with ADO.Schemas;
with ADO.Sessions;
package ADO.Audits is

   subtype Object_Key_Type is ADO.Objects.Object_Key_Type;
   subtype Class_Mapping_Access is ADO.Schemas.Class_Mapping_Access;
   subtype Column_Index is ADO.Schemas.Column_Index;

   package UBO renames Util.Beans.Objects;

   type Audit_Index is new Positive range 1 .. ADO.Configs.MAX_COLUMNS;

   type Column_Index_Array is array (Audit_Index range <>) of Column_Index;

   type Auditable_Mapping (Of_Class : ADO.Schemas.Class_Mapping_Access;
                           Count    : Audit_Index)
   is tagged limited record
      Members : Column_Index_Array (1 .. Count);
   end record;

   type Auditable_Mapping_Access is access constant Auditable_Mapping'Class;

   --  The `Audit_Info` describes a column field that is modified.
   type Audit_Info is limited record
      Field     : Column_Index := 0;
      Old_Value : UBO.Object;
      New_Value : UBO.Object;
   end record;

   type Audit_Info_Index is new Positive range 1 .. ADO.Configs.MAX_COLUMNS;

   type Audit_Array is array (Audit_Info_Index range <>) of Audit_Info;

   --  The `Auditable_Object_Record` is the root type of any auditable database record.
   --  It inherit from the `Object_Record` and adds auditing support by defining the
   --  database column fields which can be audited.  When a field is modified, it holds
   --  the audit information that tracks the old and new values.
   type Auditable_Object_Record (Key_Type   : Object_Key_Type;
                                 Of_Class   : Class_Mapping_Access;
                                 With_Audit : Auditable_Mapping_Access) is abstract
       new ADO.Objects.Object_Record with private;

   --  Release the object.
   overriding
   procedure Finalize (Object : in out Auditable_Object_Record);

   --  Record an audit information for a field.
   procedure Audit_Field (Object    : in out Auditable_Object_Record;
                          Field     : in ADO.Schemas.Column_Index;
                          Old_Value : in UBO.Object;
                          New_Value : in UBO.Object);

   --  The `Audit_Manager` is the interface of the audit manager component that is responsible
   --  for saving the audit information in the database.
   type Audit_Manager is limited interface;
   type Audit_Manager_Access is access all Audit_Manager'Class;

   --  Save the audit changes in the database.
   procedure Save (Manager : in out Audit_Manager;
                   Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in Auditable_Object_Record'Class;
                   Changes : in Audit_Array) is abstract;

   procedure Save (Session : in out ADO.Sessions.Master_Session'Class;
                   Object  : in out Auditable_Object_Record'Class);

private

   type Audit_Array_Access is access all Audit_Array;

   type Auditable_Object_Record (Key_Type   : Object_Key_Type;
                                 Of_Class   : Class_Mapping_Access;
                                 With_Audit : Auditable_Mapping_Access) is abstract
   new ADO.Objects.Object_Record (Key_Type => Key_Type, Of_Class => Of_Class) with record
      Audits : Audit_Array_Access;
   end record;

end ADO.Audits;
