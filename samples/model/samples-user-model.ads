-----------------------------------------------------------------------
--  Samples.User.Model -- Samples.User.Model
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with EL.Objects;
package Samples.User.Model is
   use Ada.Calendar;
   use Ada.Strings.Unbounded;
   use ADO.Objects;
   use ADO.Statements;
   --  --------------------
   --  Record representing a user
   --  --------------------
   type User_Ref is new ADO.Objects.Object_Ref with null record;
   --  Set the user identifier
   procedure Set_Id (Object : in out User_Ref;
                     Value  : in ADO.Identifier);
   --  Get the user identifier
   function Get_Id (Object : in User_Ref)
                 return ADO.Identifier;
   --  Set the user name
   procedure Set_Name (Object : in out User_Ref;
                       Value  : in String);
   procedure Set_Name (Object : in out User_Ref;
                       Value : in Unbounded_String);
   --  Get the user name
   function Get_Name (Object : in User_Ref)
                 return String;
   function Get_Name (Object : in User_Ref)
                 return Unbounded_String;
   --  Set the user email
   procedure Set_Email (Object : in out User_Ref;
                        Value  : in String);
   procedure Set_Email (Object : in out User_Ref;
                        Value : in Unbounded_String);
   --  Get the user email
   function Get_Email (Object : in User_Ref)
                 return String;
   function Get_Email (Object : in User_Ref)
                 return Unbounded_String;
   --  Set the user registration date
   procedure Set_Date (Object : in out User_Ref;
                       Value  : in String);
   procedure Set_Date (Object : in out User_Ref;
                       Value : in Unbounded_String);
   --  Get the user registration date
   function Get_Date (Object : in User_Ref)
                 return String;
   function Get_Date (Object : in User_Ref)
                 return Unbounded_String;
   --  Set the user description
   procedure Set_Description (Object : in out User_Ref;
                              Value  : in String);
   procedure Set_Description (Object : in out User_Ref;
                              Value : in Unbounded_String);
   --  Get the user description
   function Get_Description (Object : in User_Ref)
                 return String;
   function Get_Description (Object : in User_Ref)
                 return Unbounded_String;
   --  Set the user status
   procedure Set_Status (Object : in out User_Ref;
                         Value  : in Integer);
   --  Get the user status
   function Get_Status (Object : in User_Ref)
                 return Integer;
   --  Internal method to allocate the Object_Record instance
   procedure Allocate (Object : in out User_Ref);
   --  Copy of the object.
   function Copy (Object : User_Ref) return User_Ref;
   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);
   --  Find and load the entity.
   procedure Find (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);
   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   procedure Save (Object  : in out User_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);
   --  Delete the entity.
   procedure Delete (Object  : in out User_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);
   function Get_Value (Item : in User_Ref;
                       Name : in String) return EL.Objects.Object;
   package User_Ref_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Natural,
                                  Element_Type => User_Ref,
                                  "="          => "=");
   subtype User_Ref_Vector is User_Ref_Vectors.Vector;
   procedure List (Object  : in out User_Ref_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);
private
   USER_REF_NAME : aliased constant String := "user";
   COL_0_1_NAME : aliased constant String := "ID";
   COL_1_1_NAME : aliased constant String := "objectVersion";
   COL_2_1_NAME : aliased constant String := "NAME";
   COL_3_1_NAME : aliased constant String := "EMAIL";
   COL_4_1_NAME : aliased constant String := "DATE";
   COL_5_1_NAME : aliased constant String := "DESCRIPTION";
   COL_6_1_NAME : aliased constant String := "STATUS";
   USER_REF_TABLE : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 7,
      Table => USER_REF_NAME'Access,
      Members => (         COL_0_1_NAME'Access,
         COL_1_1_NAME'Access,
         COL_2_1_NAME'Access,
         COL_3_1_NAME'Access,
         COL_4_1_NAME'Access,
         COL_5_1_NAME'Access,
         COL_6_1_NAME'Access
)
     );
   type User_Ref_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => USER_REF_TABLE'Access)
   with record       Id : ADO.Identifier;       Object_Version : Integer;       Name : Unbounded_String;       Email : Unbounded_String;       Date : Unbounded_String;       Description : Unbounded_String;       Status : Integer;
   end record;
   type User_Ref_Access is access all User_Ref_Impl;
   overriding
   procedure Destroy (Object : access User_Ref_Impl);
   overriding
   procedure Find (Object  : in out User_Ref_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);
   procedure Load (Object  : in out User_Ref_Impl;
                   Stmt   : in out ADO.Statements.Query_Statement'Class);
   overriding
   procedure Save (Object  : in out User_Ref_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);
   procedure Create (Object  : in out User_Ref_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);
   overriding
   procedure Delete (Object  : in out User_Ref_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);
   procedure Set_Field (Object : in out User_Ref'Class;
                        Impl   : out User_Ref_Access;
                        Field  : in Positive);
end Samples.User.Model;
