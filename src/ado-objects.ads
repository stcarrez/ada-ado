-----------------------------------------------------------------------
--  ADO Objects -- Database objects
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
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Containers;

with ADO.SQL;
with ADO.Schemas;

with EL.Beans;
with EL.Objects;

with Util.Strings;
with Util.Concurrent.Counters;
limited with ADO.Sessions;
package ADO.Objects is

   --  The object was modified by a another transaction.
   --  This exception is raised by 'Save'.
   LAZY_LOCK : exception;

   INSERT_ERROR : exception;

   UPDATE_ERROR : exception;

   --  --------------------
   --  Object Key
   --  --------------------
   --  The <b>Object_Key</b> represents the primary key for an object.
   --  It is composed of the key value and a class mapping identification.
   --  The key value can be an integer or a string and is saved in the database.
   --  The class mapping is used to know in which table the object is stored.
   --  In comparison and hashing, the class mapping is used to distinguish
   --  objects of different tables.
   type Object_Key_Type is (KEY_INTEGER, KEY_STRING);
   type Object_Key (Of_Type  : Object_Key_Type;
                    Of_Class : Schemas.Class_Mapping_Access) is private;

   --  Compute the hash of the object key.
   function Hash (Key : Object_Key) return Ada.Containers.Hash_Type;

   --  Compare whether the two objects pointed to by Left and Right have the same
   --  object key.  The object key is identical if the object key type, the class
   --  mapping and the key value are identical.
   function Equivalent_Elements (Left, Right : Object_Key)
                                 return Boolean;

   --  Check if the two objects are the same database objects.
   --  The comparison is only made on the primary key.
   --  Returns true if the two objects have the same primary key.
   function "=" (Left : Object_Key; Right : Object_Key) return Boolean
     renames Equivalent_Elements;

   --  Get the key value
   function Get_Value (Key : Object_Key) return Identifier;

   --  Get the key value
   function Get_Value (Key : Object_Key) return Ada.Strings.Unbounded.Unbounded_String;

   --  Return the key value in an EL object.
   function To_Object (Key : Object_Key) return EL.Objects.Object;

   --  Get the key as a string
   function To_String (Key : Object_Key) return String;

   --  Set the key value
   procedure Set_Value (Key   : in out Object_Key;
                        Value : in Identifier);

   --  --------------------
   --  Database Object representation
   --  --------------------
   type Object_Record (Key_Type : Object_Key_Type;
                       Of_Class : ADO.Schemas.Class_Mapping_Access) is abstract
       new Ada.Finalization.Controlled with private;

   type Object_Record_Access is access all Object_Record'Class;

   --  Get the object key
   function Get_Key (Ref : in Object_Record'Class) return Object_Key;

   --  Get the object key value as a string.
   function Get_Key_Value (Ref : in Object_Record'Class)
                           return Ada.Strings.Unbounded.Unbounded_String;

   --  Get the object key value as an identifier
   function Get_Key_Value (Ref : in Object_Record'Class)
                          return Identifier;

   --  Set the object key
   procedure Set_Key (Ref : in out Object_Record'Class;
                      Key : in Object_Key);

   procedure Set_Key_Value (Ref : in out Object_Record'Class;
                            Value : in Identifier);

   procedure Set_Key_Value (Ref : in out Object_Record'Class;
                            Value : in Ada.Strings.Unbounded.Unbounded_String);

   --  Get the table name associated with the object record.
   function Get_Table_Name (Ref : in Object_Record'Class) return Util.Strings.Name_Access;

   --  Check if this is a new object.
   --  Returns True if an insert is necessary to persist this object.
   function Is_Created (Ref : in Object_Record'Class) return Boolean;

   --  Mark the object as created in the database.
   procedure Set_Created (Ref : in out Object_Record'Class);

   --  Check if the field at position <b>Field</b> was modified.
   function Is_Modified (Ref   : in Object_Record'Class;
                         Field : in Positive) return Boolean;
   pragma Inline (Is_Modified);

   --  Clear the modification flag associated with the field at
   --  position <b>Field</b>.
   procedure Clear_Modified (Ref   : in out Object_Record'Class;
                             Field : in Positive);
   pragma Inline (Clear_Modified);

   --  Frees the storage held by the object.  The database record is not deleted.
   procedure Destroy (Object : access Object_Record) is abstract;

   --  Find the object using a specific query
   procedure Find (Object  : in out Object_Record;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is abstract;

   --  Save the object in the database.
   procedure Save (Object  : in out Object_Record;
                   Session : in out ADO.Sessions.Master_Session'Class) is abstract;

   --  Create the object in the database.
   procedure Create (Object  : in out Object_Record;
                     Session : in out ADO.Sessions.Master_Session'Class) is abstract;

   --  Delete the object from the database
   procedure Delete (Object  : in out Object_Record;
                     Session : in out ADO.Sessions.Master_Session'Class) is abstract;

   --  --------------------
   --  Reference to a database object representation
   --  --------------------
   type Object_Ref is abstract
      new Ada.Finalization.Controlled and EL.Beans.Readonly_Bean with private;

   --  Mark the field identified by <b>Field</b> as modified.
   procedure Set_Field (Object : in out Object_Ref'Class;
                        Field  : in Positive);

   --  Check whether this object is initialized or not.
   function Is_Null (Object : in Object_Ref'Class) return Boolean;
   pragma Inline (Is_Null);

   --  Internal method to get the object record instance.
   function Get_Object (Ref : in Object_Ref'Class) return Object_Record_Access;
   pragma Inline (Get_Object);

   --  Check if the two objects are the same database objects.
   --  The comparison is only made on the primary key.
   --  Returns true if the two objects have the same primary key.
   function "=" (Left : Object_Ref'Class; Right : Object_Ref'Class) return Boolean;

   procedure Set_Object (Ref : in out Object_Ref'Class;
                         Object : in Object_Record_Access);

   --  Internal method to allocate the Object_Record instance
   procedure Allocate (Ref : in out Object_Ref) is abstract;

   --  Load the database object having the given identifier.
   procedure Find (Object  : in out Object_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean) is abstract;

   --  Save the object in the database.  A unique identifier is allocated
   --  if this is the first time this object is saved.
   procedure Save (Object : in out Object_Ref;
                   Save   : in out ADO.Sessions.Master_Session'Class) is abstract;

   --  Delete the object from the database.
   procedure Delete (Object  : in out Object_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class) is abstract;

   type Modified_Map is array (1 .. 64) of Boolean;
   pragma Pack (Modified_Map);

private

   type Object_Key (Of_Type  : Object_Key_Type;
                    Of_Class : ADO.Schemas.Class_Mapping_Access) is
     new Ada.Finalization.Controlled with record
      case Of_Type is
         when KEY_INTEGER =>
            Id : Identifier;

         when KEY_STRING =>
            Str : Ada.Strings.Unbounded.Unbounded_String;

      end case;
   end record;

   type Object_Ref is abstract new Ada.Finalization.Controlled
    and EL.Beans.Readonly_Bean with record
      Object : Object_Record_Access := null;
   end record;

   --  Update the reference counter.
   overriding
   procedure Adjust (Object : in out Object_Ref);

   --  Decrement the reference counter and release the object record.
   overriding
   procedure Finalize (Object : in out Object_Ref);

   --  The Object_Record represents the base class for any database object.
   --  A reference counter is used by Object_Ref to release correctly the memory.
   --
   type Object_Record (Key_Type : Object_Key_Type;
                       Of_Class : ADO.Schemas.Class_Mapping_Access) is abstract
      new Ada.Finalization.Controlled with record
      Counter    : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
      Key        : Object_Key (Of_Type => Key_Type, Of_Class => Of_Class);
      Is_Created : Boolean      := False;
      Modified   : Modified_Map := (others => False);
   end record;
--
--     type Object_Proxy is new Object_Ref with record
--        Id         : Identifier := NO_IDENTIFIER;
--        Connection :
--     end Object_Proxy;
--
--     function Get_Session (User : User_Ref_Impl) return Session_Ref is
--     begin
--        if User.Session.Object then
--           return Session_Ref '(Object => User.Session.Object);
--        end if;
--        User.Session.Object := new Session_Ref_Impl;
--        User_Session.Object.Load (User.Session.Connection);
--
--     end Get_Session;

end ADO.Objects;
