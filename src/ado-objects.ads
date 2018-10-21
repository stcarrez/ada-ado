-----------------------------------------------------------------------
--  ADO Objects -- Database objects
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2018 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Beans.Objects;

with Util.Strings;
with Util.Concurrent.Counters;
limited with ADO.Sessions;

--  = Objects =
--  When a database table is represented by an Ada type, the `ADO.Objects.Object_Record`
--  tagged record is used as the root type for the model representation.  The type provides
--  operations to.
--
--  The `Object_Ref` type is the root type of any database record reference.
--
--  == Object Creation ==
--
--  == Object Deletion ==
--
--  @include ado-sequences.ads
package ADO.Objects is

   --  The object was modified by a another transaction.
   --  This exception is raised by 'Save'.
   LAZY_LOCK    : exception;

   INSERT_ERROR : exception;

   UPDATE_ERROR : exception;

   --  The object record was not found in the database.
   NOT_FOUND       : exception;

   --  The object is NULL.
   NULL_ERROR : exception;

   --  --------------------
   --  Object Key
   --  --------------------
   --  The <b>Object_Key</b> represents the primary key for an object.
   --  It is composed of the key value and a class mapping identification.
   --  The key value can be an integer or a string and is saved in the database.
   --  The class mapping is used to know in which table the object is stored.
   --  In comparison and hashing, the class mapping is used to distinguish
   --  objects of different tables.
   --
   --  Limitations:
   --  --------------------
   --  o The primary key must be a single column represented as an integer or a string
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

   --  Return the key value in a bean object.
   function To_Object (Key : Object_Key) return Util.Beans.Objects.Object;

   --  Get the key as a string
   function To_String (Key : Object_Key) return String;

   --  Set the key value
   procedure Set_Value (Key   : in out Object_Key;
                        Value : in Identifier);

   --  Set the key value
   procedure Set_Value (Key   : in out Object_Key;
                        Value : in String);

   --  --------------------
   --  Database Object representation
   --  --------------------
   --  The <b>Object_Record</b> is the root type of any database record row.
   --  It holds the primary key as well as the class mapping associated with the record.
   --  Applications do not use the <b>Object_Record</b> directly but instead they receive
   --  and use an <b>Object_Ref</b>.
   type Object_Record (Key_Type : Object_Key_Type;
                       Of_Class : ADO.Schemas.Class_Mapping_Access) is abstract
       new Ada.Finalization.Limited_Controlled with private;

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

   procedure Set_Key_Value (Ref   : in out Object_Record'Class;
                            Value : in Identifier);

   procedure Set_Key_Value (Ref   : in out Object_Record'Class;
                            Value : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Set_Key_Value (Ref   : in out Object_Record'Class;
                            Value : in String);

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

   --  Release the object.
   overriding
   procedure Finalize (Object : in out Object_Record);

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

   --  Load the object from the database.  The object has been lazy loaded and only its
   --  primary key is known.  This is called from <b>Lazy_Load</b>.
   procedure Load (Object  : in out Object_Record;
                   Session : in out ADO.Sessions.Session'Class) is abstract;

   --  Copy the source object record into the target.
   procedure Copy (To   : in out Object_Record;
                   From : in Object_Record'Class);

   --  --------------------
   --  Reference to a database object representation
   --  --------------------
   --  The <b>Object_Ref</b> is the root type of any database record reference.
   --  The reference is the object that applications can use to access the record object.
   type Object_Ref is abstract
      new Ada.Finalization.Controlled and Util.Beans.Basic.Readonly_Bean with private;

   --  Mark the field identified by <b>Field</b> as modified.
   procedure Set_Field (Object : in out Object_Ref'Class;
                        Field  : in Positive);

   --  Prepare the object to be modified.  If the reference is empty, an object record
   --  instance is allocated by calling <b>Allocate</b>.
   procedure Prepare_Modify (Object : in out Object_Ref'Class;
                             Result : out Object_Record_Access);

   --  Check whether this object is initialized or not.
   function Is_Null (Object : in Object_Ref'Class) return Boolean;
   pragma Inline (Is_Null);

   --  Check whether this object is saved in the database.
   --  Returns True if the object was saved in the database.
   function Is_Inserted (Object : in Object_Ref'Class) return Boolean;

   --  Check whether this object is loaded from the database.
   function Is_Loaded (Object : in Object_Ref'Class) return Boolean;

   --  Internal method to get the object record instance and make sure it is fully loaded.
   --  If the object was not yet loaded, calls <b>Lazy_Load</b> to get the values from the
   --  database.  Raises Session_Error if the session associated with the object is closed.
   function Get_Load_Object (Ref : in Object_Ref'Class) return Object_Record_Access;
   pragma Inline (Get_Load_Object);

   --  Internal method to get the object record instance.
   function Get_Object (Ref : in Object_Ref'Class) return Object_Record_Access;
   pragma Inline (Get_Object);

   --  Get the object key
   function Get_Key (Ref : in Object_Ref'Class) return Object_Key;

   --  Set the object key.
   procedure Set_Key_Value (Ref     : in out Object_Ref'Class;
                            Value   : in Identifier;
                            Session : in ADO.Sessions.Session'Class);

   --  Set the object key.
   procedure Set_Key_Value (Ref     : in out Object_Ref'Class;
                            Value   : in Ada.Strings.Unbounded.Unbounded_String;
                            Session : in ADO.Sessions.Session'Class);

   --  Check if the two objects are the same database objects.
   --  The comparison is only made on the primary key.
   --  Returns true if the two objects have the same primary key.
   function "=" (Left : Object_Ref; Right : Object_Ref) return Boolean;

   procedure Set_Object (Ref    : in out Object_Ref'Class;
                         Object : in Object_Record_Access);

   procedure Set_Object (Ref     : in out Object_Ref'Class;
                         Object  : in Object_Record_Access;
                         Session : in ADO.Sessions.Session'Class);

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

   type Session_Proxy is limited private;
   type Session_Proxy_Access is access all Session_Proxy;

   --  Release the session proxy, deleting the instance if it is no longer used.
   --  The <tt>Detach</tt> parameter controls whether the session proxy must be detached
   --  from the database session.  When set, the session proxy is no longer linked to the
   --  database session and trying to load the lazy object will raise the Session_Error
   --  exception.
   procedure Release_Proxy (Proxy  : in out Session_Proxy_Access;
                            Detach : in Boolean := False);
   pragma Inline (Release_Proxy);

   function Create_Session_Proxy (S : access ADO.Sessions.Session_Record)
                                  return Session_Proxy_Access;

   --  Set the object field to the new value in <b>Into</b>.  If the new value is identical,
   --  the operation does nothing.  Otherwise, the new value <b>Value</b> is copied
   --  to <b>Into</b> and the field identified by <b>Field</b> is marked as modified on
   --  the object.  The <b>Set_Field_XXX</b> procedures are used by the Dynamo generated
   --  code for the implementation of Set procedures.
   procedure Set_Field_Unbounded_String (Object : in out Object_Record'Class;
                                         Field  : in Positive;
                                         Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                                         Value  : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Positive;
                               Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                               Value  : in String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Positive;
                               Into   : in out ADO.Nullable_String;
                               Value  : in String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Positive;
                               Into   : in out ADO.Nullable_String;
                               Value  : in ADO.Nullable_String);

   procedure Set_Field_Time (Object : in out Object_Record'Class;
                             Field  : in Positive;
                             Into   : in out Ada.Calendar.Time;
                             Value  : in Ada.Calendar.Time);

   procedure Set_Field_Time (Object : in out Object_Record'Class;
                             Field  : in Positive;
                             Into   : in out ADO.Nullable_Time;
                             Value  : in ADO.Nullable_Time);

   procedure Set_Field_Integer (Object : in out Object_Record'Class;
                                Field  : in Positive;
                                Into   : in out Integer;
                                Value  : in Integer);

   procedure Set_Field_Integer (Object : in out Object_Record'Class;
                                Field  : in Positive;
                                Into   : in out ADO.Nullable_Integer;
                                Value  : in ADO.Nullable_Integer);

   procedure Set_Field_Natural (Object : in out Object_Record'Class;
                                Field  : in Positive;
                                Into   : in out Natural;
                                Value  : in Natural);

   procedure Set_Field_Positive (Object : in out Object_Record'Class;
                                 Field  : in Positive;
                                 Into   : in out Positive;
                                 Value  : in Positive);

   procedure Set_Field_Boolean (Object : in out Object_Record'Class;
                                Field  : in Positive;
                                Into   : in out Boolean;
                                Value  : in Boolean);

   procedure Set_Field_Object (Object : in out Object_Record'Class;
                               Field  : in Positive;
                               Into   : in out Object_Ref'Class;
                               Value  : in Object_Ref'Class);

   procedure Set_Field_Identifier (Object : in out Object_Record'Class;
                                   Field  : in Positive;
                                   Into   : in out ADO.Identifier;
                                   Value  : in ADO.Identifier);

   procedure Set_Field_Entity_Type (Object : in out Object_Record'Class;
                                    Field  : in Positive;
                                    Into   : in out ADO.Entity_Type;
                                    Value  : in ADO.Entity_Type);

   procedure Set_Field_Entity_Type (Object : in out Object_Record'Class;
                                    Field  : in Positive;
                                    Into   : in out ADO.Nullable_Entity_Type;
                                    Value  : in ADO.Nullable_Entity_Type);

   procedure Set_Field_Blob (Object : in out Object_Record'Class;
                             Field  : in Positive;
                             Into   : in out ADO.Blob_Ref;
                             Value  : in ADO.Blob_Ref);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Positive;
                                  Value  : in ADO.Identifier);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Positive;
                                  Value  : in String);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Positive;
                                  Value  : in Ada.Strings.Unbounded.Unbounded_String);

   generic
      type T is private;
   procedure Set_Field_Operation (Object : in out Object_Record'Class;
                                  Field  : in Positive;
                                  Into   : in out T;
                                  Value  : in T);
   --  SCz 2011-11-15: ??? setting the Inline_Always pragma crashes gcc 4.4;
   --     pragma Inline_Always (Set_Field_Operation);

private

   --  Load the object from the database if it was not already loaded.
   --  For a lazy association, the <b>Object_Record</b> is allocated and holds the primary key.
   --  The <b>Is_Loaded</b> boolean is cleared thus indicating the other values are not loaded.
   --  This procedure makes sure these values are loaded by invoking <b>Load</b> if necessary.
   --  Raises Session_Error if the session associated with the object is closed.
   procedure Lazy_Load (Ref : in Object_Ref'Class);

   type Object_Key (Of_Type  : Object_Key_Type;
                    Of_Class : ADO.Schemas.Class_Mapping_Access) is
     new Ada.Finalization.Controlled with record
      case Of_Type is
         when KEY_INTEGER =>
            Id : Identifier := NO_IDENTIFIER;

         when KEY_STRING =>
            Str : Ada.Strings.Unbounded.Unbounded_String;

      end case;
   end record;

   type Object_Ref is abstract new Ada.Finalization.Controlled
    and Util.Beans.Basic.Readonly_Bean with record
      Object : Object_Record_Access := null;
   end record;

   --  Update the reference counter.
   overriding
   procedure Adjust (Object : in out Object_Ref);

   --  Decrement the reference counter and release the object record.
   overriding
   procedure Finalize (Object : in out Object_Ref);

   type Session_Proxy is limited record
      Counter : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
      Session : access ADO.Sessions.Session_Record;
   end record;

   --  The Object_Record represents the base class for any database object.
   --  A reference counter is used by Object_Ref to release correctly the memory.
   --
   type Object_Record (Key_Type : Object_Key_Type;
                       Of_Class : ADO.Schemas.Class_Mapping_Access) is
     abstract new Ada.Finalization.Limited_Controlled with record
      Counter    : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
      Session    : Session_Proxy_Access := null;
      Key        : Object_Key (Of_Type => Key_Type, Of_Class => Of_Class);
      Is_Created : Boolean      := False;
      Is_Loaded  : Boolean      := False;
      Modified   : Modified_Map := (others => False);
   end record;

end ADO.Objects;
