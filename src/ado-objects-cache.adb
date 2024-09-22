-----------------------------------------------------------------------
--  objects.cache -- Object cache
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>ADO.Objects.Cache</b> holds a cache of object records.
--  The cache maintains a set of objects that were loaded for a given session.
--
--  The cache is not thread-safe.
package body ADO.Objects.Cache is

   --  ------------------------------
   --  Insert the object in the cache.
   --  The reference counter associated with the object is incremented.
   --  ------------------------------
   procedure Insert (Cache  : in out Object_Cache;
                     Object : in Object_Record_Access) is
      Pos : constant Object_Set.Cursor := Cache.Objects.Find (Object);
   begin
      if Object_Set.Has_Element (Pos) then
         declare
            Current : constant Object_Record_Access := Object_Set.Element (Pos);
            Is_Zero : Boolean;
         begin
            if Current = Object then
               return;
            end if;
            Cache.Objects.Replace_Element (Pos, Object);
            Util.Concurrent.Counters.Decrement (Current.Counter, Is_Zero);
            if Is_Zero then
               Destroy (Current);
            end if;
         end;
      else
         Cache.Objects.Insert (Object);
      end if;
      Util.Concurrent.Counters.Increment (Object.Counter);
   end Insert;

   --  ------------------------------
   --  Insert the object in the cache
   --  The reference counter associated with the object is incremented.
   --  ------------------------------
   procedure Insert (Cache  : in out Object_Cache;
                     Object : in Object_Ref'Class) is
   begin
      if Object.Object /= null then
         Insert (Cache, Object.Object);
      end if;
   end Insert;

   --  ------------------------------
   --  Check if the object is contained in the cache
   --  ------------------------------
   function Contains (Cache : in Object_Cache;
                      Key   : in Object_Key) return Boolean is
      pragma Unreferenced (Cache, Key);
   begin
      return False;
   end Contains;

   function Find (Cache : in Object_Cache;
                  Key   : in Object_Key) return Object_Record_Access is
      pragma Unreferenced (Cache, Key);
   begin
      return null;
   end Find;

   procedure Find (Cache  : in Object_Cache;
                   Object : in out Object_Ref'Class;
                   Key    : in Object_Key) is
      pragma Unreferenced (Cache, Object, Key);
   begin
      null;
   end Find;

   --  ------------------------------
   --  Remove the object from the cache.  The reference counter associated
   --  with the object is decremented.
   --  Do nothing if the object is not in the cache.
   --  ------------------------------
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Record_Access) is
      Pos : Object_Set.Cursor := Cache.Objects.Find (Object);
   begin
      if Object_Set.Has_Element (Pos) then
         declare
            Current : constant Object_Record_Access := Object_Set.Element (Pos);
            Is_Zero : Boolean;
         begin
            Cache.Objects.Delete (Pos);
            Util.Concurrent.Counters.Decrement (Current.Counter, Is_Zero);
            if Is_Zero then
               Destroy (Current);
            end if;
         end;
      end if;
   end Remove;

   --  ------------------------------
   --  Remove all object in the cache.
   --  ------------------------------
   procedure Remove_All (Cache : in out Object_Cache) is
      pragma Unreferenced (Cache);
   begin
      null;
   end Remove_All;

   --  ------------------------------
   --  Remove the object from the cache.
   --  Do nothing if the object is not in the cache.
   --  ------------------------------
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Ref) is
   begin
      if Object.Object /= null then
         Remove (Cache, Object.Object);
      end if;
   end Remove;

   --  ------------------------------
   --  Remove the object from the cache.
   --  Do nothing if the object is not in the cache.
   --  ------------------------------
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Key) is
      pragma Unreferenced (Cache, Object);
--        Item : aliased Object_Record (Key_Type => Object.Of_Type);
   begin
--        Item.Key := Object;
--        Remove (Cache, Item'Unchecked_Access);
      null;
   end Remove;

   --  ------------------------------
   --  The cache is a <b>Hashed_Set</b> in which the object record pointers
   --  are stored.  The hash and comparison methods are based on the object key
   --  and object type.
   --  ------------------------------
   function Hash (Item : Object_Record_Access) return Hash_Type is
   begin
      if Item = null then
         return 0;
      else
         return Hash (Item.Key);
      end if;
   end Hash;

   --  ------------------------------
   --  Compare whether the two objects pointed to by Left and Right have the same
   --  object key.
   --  ------------------------------
   function Equivalent_Elements (Left, Right : Object_Record_Access)
                                 return Boolean is
   begin
      if Left = Right then
         return True;
      end if;
      if Left = null or else Right = null then
         return False;
      end if;
      if Left.all'Size /= Right.all'Size then
         return False;
      end if;
      if Left.Key /= Right.Key then
         return False;
      end if;

      return True;
   end Equivalent_Elements;

   --  ------------------------------
   --  Finalize the object cache by removing all entries
   --  ------------------------------
   overriding
   procedure Finalize (Cache : in out Object_Cache) is
   begin
      Remove_All (Cache);
   end Finalize;

end ADO.Objects.Cache;
