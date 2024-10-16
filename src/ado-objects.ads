-----------------------------------------------------------------------
--  ado-objects -- Database objects
--  Copyright (C) 2009 - 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

--  == Objects ==
--  When a database table is mapped into an Ada object, the application holds a reference
--  to that object through the `Object_Ref` type.
--  The `Object_Ref` tagged type is the root type of any database record reference.
--  Reference counting is used so that the object can be stored, shared and the memory
--  management is handled automatically.  It defines generic operations to be able to:
--
--    * load the database record and map it to the Ada object,
--    * save the Ada object into the database either by inserting or updating it,
--    * delete the database record.
--
--  The Dynamo code generator will generate a specific tagged type for each database table
--  that is mapped.  These tagged type will inherit from the `Object_Ref` and will implement
--  the required abstract operations.  For each of them, the code generator will generate
--  the `Get_X` and `Set_X` operation for each column mapped in Ada.
--
--  Before the `Object_Ref` is a reference, it does not hold the database record itself.
--  The `ADO.Objects.Object_Record` tagged record is used for that and it defines the
--  root type for the model representation.  The type provides operations to modify a
--  data field of the record while tracking its changes so that when the `Save` operation
--  is called, only the data fields that have been modified are updated in the database.
--  An application will not use nor access the `Object_Record`.  The Dynamo code generator
--  generates a private type to make sure it is only accessed through the reference.
--
--  Several predicate operations are available to help applications check the validity
--  of an object reference:
--
--  | Function    | Description |
--  | ----------- |--------------------------------------------------------- |
--  | Is_Null     | When returning True, it indicates the reference is NULL. |
--  | Is_Loaded   | When returning True, it indicates the object was loaded from the database. |
--  | Is_Inserted | When returning True, it indicates the object was inserted in the database. |
--  | Is_Modified | When returning True, it indicates the object was modified and must be saved. |
--
--  Let's assume we have a `User_Ref` mapped record, an instance of the reference would
--  be declared as follows:
--
--    with Samples.User.Model;
--    ...
--      User : Samples.User.Model.User_Ref;
--
--  After this declaration, the reference is null and the following assumption is true:
--
--    User.Is_Null and not User.Is_Loaded and not User.Is_Inserted
--
--  If we set a data field such as the name, an object is allocated and the reference
--  is no longer null.
--
--    User.Set_Name ("Ada Lovelace");
--
--  After this statement, the following assumption is true:
--
--    not User.Is_Null and not User.Is_Loaded and not User.Is_Inserted
--
--  With this, it is therefore possible to identify that this object is not yet
--  saved in the database.  After calling the `Save` procedure, a primary key is
--  allocated and the following assumption becomes true:
--
--    not User.Is_Null and not User.Is_Loaded and User.Is_Inserted
--
--  == Loading Objects ==
--  Three operations are generated by the Dynamo code generator to help in loading
--  a object from the database: two `Load` procedures and a `Find` procedure.
--  The `Load` procedures are able to load an object by using its primary key.
--  Two forms of `Load` are provided: one that raises the `ADO.Objects.NOT_FOUND`
--  exception and another that returns an additional `Found` boolean parameter.
--  Within the application, if the database row is expected to exist, the first
--  form should be used.  In other cases, when the application expects that the
--  database record may not exist, the second form is easier and avoids raising
--  and handling an exception for a common case.
--
--     User.Load (Session, 1234);
--
--  The `Find` procedure allows to retrieve a database record by specifying a
--  filter.  The filter object is represented by the `ADO.SQL.Query` tagged record.
--  A simple query filter is declared as follows:
--
--    Filter : ADO.SQL.Query;
--
--  The filter is an SQL fragment that is inserted within the `WHERE` clause to
--  find the object record.  The filter can use parameters that are configured
--  by using the `Bind_Param` or `Add_Param` operations.  For example, to find
--  a user from its name, the following filter could be set:
--
--    Filter.Set_Filter ("name = :name");
--    Filter.Bind_Param ("name", "Ada Lovelace");
--
--  Once the query filter is initialized and configured with its parameters,
--  the `Find` procedure can be called:
--
--    Found : Boolean;
--    ...
--    User.Find (Session, Filter, Found);
--
--  The `Find` procedure does not raise an exception if the database record is not found.
--  Instead, it returns a boolean status in the `Found` output parameter.  The `Find`
--  procedure will execute an SQL `SELECT` statement with a `WHERE` clause to retrieve
--  the database record.  The `Found` output parameter is set when the query returns
--  exactly one row.
--
--  == Modifying Objects ==
--  To modify an object, applications will use one of the `Set_X` operation generated
--  for each mapped column.  The ADO runtime will keep track of which data fields are
--  modified.  The `Save` procedure must be called to update the database record.
--  When calling it, an SQL `UPDATE` statement is generated to update the modified
--  data fields.
--
--    User.Set_Status (1);
--    User.Save (Session);
--
--  == Deleting Objects ==
--  Deleting objects is made by using the `Delete` operation.
--
--    User.Delete (Session);
--
--  Sometimes you may want to delete an object without having to load it first.
--  This is possible by delete an object without loading it.  For this, set the
--  primary key on the object and call the `Delete` operation:
--
--    User.Set_Id (42);
--    User.Delete (Session);
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
   overriding
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

   subtype Column_Index is ADO.Schemas.Column_Index;

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

   --  Check if at least one field is modified and the object must be saved.
   function Is_Modified (Ref : in Object_Record'Class) return Boolean;

   --  Check if the field at position <b>Field</b> was modified.
   function Is_Modified (Ref   : in Object_Record'Class;
                         Field : in Column_Index) return Boolean with Inline;

   --  Clear the modification flag associated with the field at
   --  position <b>Field</b>.
   procedure Clear_Modified (Ref   : in out Object_Record'Class;
                             Field : in Column_Index) with Inline;

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
                        Field  : in Column_Index);

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

   --  Check if at least one field is modified and the object must be saved.
   function Is_Modified (Object : in Object_Ref'Class) return Boolean;

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
   overriding
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

   --  Get the object primary key in a bean object.
   function To_Object (Object : in Object_Ref'Class) return Util.Beans.Objects.Object;

   type Modified_Map is array (Column_Index range 1 .. 64) of Boolean;
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
                                         Field  : in Column_Index;
                                         Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                                         Value  : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out Ada.Strings.Unbounded.Unbounded_String;
                               Value  : in String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out ADO.Nullable_String;
                               Value  : in String);

   procedure Set_Field_String (Object : in out Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out ADO.Nullable_String;
                               Value  : in ADO.Nullable_String);

   procedure Set_Field_Time (Object : in out Object_Record'Class;
                             Field  : in Column_Index;
                             Into   : in out Ada.Calendar.Time;
                             Value  : in Ada.Calendar.Time);

   procedure Set_Field_Time (Object : in out Object_Record'Class;
                             Field  : in Column_Index;
                             Into   : in out ADO.Nullable_Time;
                             Value  : in ADO.Nullable_Time);

   procedure Set_Field_Integer (Object : in out Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Integer;
                                Value  : in Integer);

   procedure Set_Field_Integer (Object : in out Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out ADO.Nullable_Integer;
                                Value  : in ADO.Nullable_Integer);

   procedure Set_Field_Natural (Object : in out Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Natural;
                                Value  : in Natural);

   procedure Set_Field_Positive (Object : in out Object_Record'Class;
                                 Field  : in Column_Index;
                                 Into   : in out Positive;
                                 Value  : in Positive);

   procedure Set_Field_Boolean (Object : in out Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Boolean;
                                Value  : in Boolean);

   procedure Set_Field_Boolean (Object : in out Object_Record'Class;
                                Field  : in Column_Index;
                                Into   : in out Nullable_Boolean;
                                Value  : in Nullable_Boolean);

   procedure Set_Field_Float (Object : in out Object_Record'Class;
                              Field  : in Column_Index;
                              Into   : in out Float;
                              Value  : in Float);

   procedure Set_Field_Long_Float (Object : in out Object_Record'Class;
                                   Field  : in Column_Index;
                                   Into   : in out Long_Float;
                                   Value  : in Long_Float);

   procedure Set_Field_Object (Object : in out Object_Record'Class;
                               Field  : in Column_Index;
                               Into   : in out Object_Ref'Class;
                               Value  : in Object_Ref'Class);

   procedure Set_Field_Identifier (Object : in out Object_Record'Class;
                                   Field  : in Column_Index;
                                   Into   : in out ADO.Identifier;
                                   Value  : in ADO.Identifier);

   procedure Set_Field_Entity_Type (Object : in out Object_Record'Class;
                                    Field  : in Column_Index;
                                    Into   : in out ADO.Entity_Type;
                                    Value  : in ADO.Entity_Type);

   procedure Set_Field_Entity_Type (Object : in out Object_Record'Class;
                                    Field  : in Column_Index;
                                    Into   : in out ADO.Nullable_Entity_Type;
                                    Value  : in ADO.Nullable_Entity_Type);

   procedure Set_Field_Blob (Object : in out Object_Record'Class;
                             Field  : in Column_Index;
                             Into   : in out ADO.Blob_Ref;
                             Value  : in ADO.Blob_Ref);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in ADO.Identifier);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in String);

   procedure Set_Field_Key_Value (Object : in out Object_Record'Class;
                                  Field  : in Column_Index;
                                  Value  : in Ada.Strings.Unbounded.Unbounded_String);

   generic
      type T is private;
   procedure Set_Field_Operation (Object : in out Object_Record'Class;
                                  Field  : in Column_Index;
                                  Into   : in out T;
                                  Value  : in T);
   --  SCz 2011-11-15: ??? setting the Inline_Always pragma crashes gcc 4.4;
   --     pragma Inline_Always (Set_Field_Operation);

   --  Mark the field identified by <b>Field</b> as modified.
   procedure Set_Field (Object : in out Object_Record'Class;
                        Field  : in Column_Index);

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
