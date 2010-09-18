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

package body ADO.Objects is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Objects");

   --  ------------------------------
   --  Compute the hash of the object key.
   --  ------------------------------
   function Hash (Key : Object_Key) return Ada.Containers.Hash_Type is
      Result : Ada.Containers.Hash_Type;
   begin
      --  @todo SCz 2010-08-03: hash on the object type/class/table
      case Key.Of_Type is
         when KEY_INTEGER =>
            Result := Ada.Containers.Hash_Type (Key.Id);

         when KEY_STRING =>
            Result := 0;

      end case;
      return Result;
   end Hash;

   --  ------------------------------
   --  Compare whether the two objects pointed to by Left and Right have the same
   --  object key.
   --  ------------------------------
   function Equivalent_Elements (Left, Right : Object_Key)
                                 return Boolean is
      use Ada.Strings.Unbounded;
   begin
      if Left.Of_Type /= Right.Of_Type then
         return False;
      end if;
      case Left.Of_Type is
         when KEY_INTEGER =>
            return Left.Id = Right.Id;

         when KEY_STRING =>
            return Left.Str = Right.Str;

      end case;
   end Equivalent_Elements;

   procedure Adjust (Object : in out Object_Ref) is
   begin
      if Object.Object /= null then
         Util.Concurrent.Counters.Increment (Object.Object.Counter);
      end if;
   end Adjust;

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
   --  Check if the two objects are the same database objects.
   --  The comparison is only made on the primary key.
   --  Returns true if the two objects have the same primary key.
   --  ------------------------------
   function "=" (Left : Object_Ref'Class; Right : Object_Ref'Class) return Boolean is
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
   function Get_Id (Ref : in Object_Record'Class) return Object_Key is
   begin
      return Ref.Key;
   end Get_Id;

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
