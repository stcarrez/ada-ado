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

with Util.Log;
with Util.Log.Loggers;
with Ada.Strings.Unbounded.Hash;

package body ADO.Objects is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Objects");
   use type ADO.Schemas.Class_Mapping_Access;

   --  ------------------------------
   --  Compute the hash of the object key.
   --  ------------------------------
   function Hash (Key : Object_Key) return Ada.Containers.Hash_Type is
      use Ada.Containers;

      Result : Ada.Containers.Hash_Type;
   begin
      case Key.Of_Type is
         when KEY_INTEGER =>
            if Key.Id < 0 then
               Result := Hash_Type (-Key.Id);
            else
               Result := Hash_Type (Key.Id);
            end if;

         when KEY_STRING =>
            Result := Ada.Strings.Unbounded.Hash (Key.Str);

      end case;

      --  Merge with the class mapping hash so that two key values of different
      --  tables will result in a different hash.
      Result := Result xor ADO.Schemas.Hash (Key.Of_Class);
      return Result;
   end Hash;

   --  ------------------------------
   --  Compare whether the two objects pointed to by Left and Right have the same
   --  object key.  The object key is identical if the object key type, the class
   --  mapping and the key value are identical.
   --  ------------------------------
   function Equivalent_Elements (Left, Right : Object_Key)
                                 return Boolean is
      use Ada.Strings.Unbounded;
   begin
      if Left.Of_Type /= Right.Of_Type then
         return False;
      end if;
      if Left.Of_Class /= Right.Of_Class then
         return False;
      end if;
      case Left.Of_Type is
         when KEY_INTEGER =>
            return Left.Id = Right.Id;

         when KEY_STRING =>
            return Left.Str = Right.Str;

      end case;
   end Equivalent_Elements;

   --  ------------------------------
   --  Get the key value
   --  ------------------------------
   function Get_Value (Key : Object_Key) return Identifier is
   begin
      return Key.Id;
   end Get_Value;

   --  ------------------------------
   --  Get the key value
   --  ------------------------------
   function Get_Value (Key : Object_Key) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Key.Str;
   end Get_Value;

   --  ------------------------------
   --  Set the key value
   --  ------------------------------
   procedure Set_Value (Key   : in out Object_Key;
                        Value : in Identifier) is
   begin
      case Key.Of_Type is
         when KEY_INTEGER =>
            Key.Id := Value;

         when KEY_STRING =>
            Key.Str := Ada.Strings.Unbounded.To_Unbounded_String (Identifier'Image (Value));

      end case;
   end Set_Value;

   --  ------------------------------
   --  Get the key as a string
   --  ------------------------------
   function To_String (Key : Object_Key) return String is
   begin
      case Key.Of_Type is
         when KEY_INTEGER =>
            return Identifier'Image (Key.Id);

         when KEY_STRING =>
            return Ada.Strings.Unbounded.To_String (Key.Str);

      end case;
   end To_String;

   --  ------------------------------
   --  Return the key value in an EL object.
   --  ------------------------------
   function To_Object (Key : Object_Key) return EL.Objects.Object is
   begin
      case Key.Of_Type is
         when KEY_INTEGER =>
            return EL.Objects.To_Object (Long_Long_Integer (Key.Id));

         when KEY_STRING =>
            return EL.Objects.To_Object (Key.Str);

      end case;
   end To_Object;

   --  ------------------------------
   --  Increment the reference counter when an object is copied
   --  ------------------------------
   overriding
   procedure Adjust (Object : in out Object_Ref) is
   begin
      if Object.Object /= null then
         Util.Concurrent.Counters.Increment (Object.Object.Counter);
      end if;
   end Adjust;

   --  ------------------------------
   --  Decrement the reference counter and release the object record.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Object_Ref) is
      Is_Zero : Boolean;
   begin
      if Object.Object /= null then
         Util.Concurrent.Counters.Decrement (Object.Object.Counter, Is_Zero);
         if Is_Zero then
            Destroy (Object.Object);
            Object.Object := null;
         end if;
      end if;
   end Finalize;

   --  ------------------------------
   --  Mark the field identified by <b>Field</b> as modified.
   --  ------------------------------
   procedure Set_Field (Object : in out Object_Ref'Class;
                        Field  : in Positive) is
   begin
      if Object.Object = null then
         Object.Allocate;
      end if;
      Object.Object.Modified (Field) := True;
   end Set_Field;

   --  ------------------------------
   --  Check whether this object is initialized or not.
   --  ------------------------------
   function Is_Null (Object : in Object_Ref'Class) return Boolean is
   begin
      return Object.Object = null;
   end Is_Null;

   --  ------------------------------
   --  Internal method to get the object record instance.
   --  ------------------------------
   function Get_Object (Ref : in Object_Ref'Class) return Object_Record_Access is
   begin
      return Ref.Object;
   end Get_Object;

   --  ------------------------------
   --  Get the object key
   --  ------------------------------
   function Get_Key (Ref : in Object_Ref'Class) return Object_Key is
   begin
      return Ref.Object.Key;
   end Get_Key;

   --  ------------------------------
   --  Check if the two objects are the same database objects.
   --  The comparison is only made on the primary key.
   --  Returns true if the two objects have the same primary key.
   --  ------------------------------
   function "=" (Left : Object_Ref; Right : Object_Ref) return Boolean is
   begin
      --  Same target object
      if Left.Object = Right.Object then
         return True;
      end if;
      --  One of the target object is null
      if Left.Object = null or Right.Object = null then
         return False;
      end if;
      return Left.Object.Key = Right.Object.Key;
   end "=";

   procedure Set_Object (Ref : in out Object_Ref'Class;
                         Object : in Object_Record_Access) is
      Is_Zero : Boolean;
   begin
      if Ref.Object /= null and Ref.Object /= Object then
         Util.Concurrent.Counters.Decrement (Ref.Object.Counter, Is_Zero);
         if Is_Zero then
            Destroy (Ref.Object);
         end if;
      end if;
      Ref.Object := Object;
   end Set_Object;

   --  ------------------------------
   --  Get the object key
   --  ------------------------------
   function Get_Key (Ref : in Object_Record'Class) return Object_Key is
   begin
      return Ref.Key;
   end Get_Key;

   --  ------------------------------
   --  Set the object key
   --  ------------------------------
   procedure Set_Key (Ref : in out Object_Record'Class;
                      Key : in Object_Key) is
   begin
      Ref.Key := Key;
   end Set_Key;

   --  ------------------------------
   --  Get the object key value as an identifier
   --  ------------------------------
   function Get_Key_Value (Ref : in Object_Record'Class)
                           return Identifier is
   begin
      return Ref.Key.Id;
   end Get_Key_Value;

   function Get_Key_Value (Ref : in Object_Record'Class) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ref.Key.Str;
   end Get_Key_Value;

   procedure Set_Key_Value (Ref : in out Object_Record'Class;
                            Value : in Identifier) is
   begin
      Set_Value (Ref.Key, Value);
   end Set_Key_Value;

   procedure Set_Key_Value (Ref : in out Object_Record'Class;
                            Value : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Ref.Key.Str := Value;
   end Set_Key_Value;

   --  ------------------------------
   --  Get the table name associated with the object record.
   --  ------------------------------
   function Get_Table_Name (Ref : in Object_Record'Class) return Util.Strings.Name_Access is
   begin
      if Ref.Key.Of_Class = null then
         return null;
      else
         return Ref.Key.Of_Class.Table;
      end if;
   end Get_Table_Name;

   --  ------------------------------
   --  Check if this is a new object.
   --  Returns True if an insert is necessary to persist this object.
   --  ------------------------------
   function Is_Created (Ref : in Object_Record'Class) return Boolean is
   begin
      return Ref.Is_Created;
   end Is_Created;

   --  ------------------------------
   --  Mark the object as created in the database.
   --  ------------------------------
   procedure Set_Created (Ref : in out Object_Record'Class) is
   begin
      Ref.Is_Created := True;
   end Set_Created;

   --  ------------------------------
   --  Check if the field at position <b>Field</b> was modified.
   --  ------------------------------
   function Is_Modified (Ref   : in Object_Record'Class;
                         Field : in Positive) return Boolean is
   begin
      return Ref.Modified (Field);
   end Is_Modified;

   --  ------------------------------
   --  Clear the modification flag associated with the field at
   --  position <b>Field</b>.
   --  ------------------------------
   procedure Clear_Modified (Ref   : in out Object_Record'Class;
                             Field : in Positive) is
   begin
      Ref.Modified (Field) := False;
   end Clear_Modified;

end ADO.Objects;
