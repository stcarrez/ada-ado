-----------------------------------------------------------------------
--  objects.cache -- Object cache
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

with ADO.Objects;
with Ada.Containers.Indefinite_Hashed_Sets;

--  The <b>ADO.Objects.Cache</b> holds a cache of object records.
--  The cache maintains a set of objects that were loaded for a given session.
--
--  The cache is not thread-safe.  It is not intended to be shared directly.
package ADO.Objects.Cache is

   type Object_Cache is limited private;

   --  Insert the object in the cache.
   --  The reference counter associated with the object is incremented.
   procedure Insert (Cache  : in out Object_Cache;
                     Object : in Object_Record_Access);

   --  Insert the object in the cache
   --  The reference counter associated with the object is incremented.
   procedure Insert (Cache  : in out Object_Cache;
                     Object : in Object_Ref'Class);

   --  Check if the object is contained in the cache
   function Contains (Cache : in Object_Cache;
                      Key   : in Object_Key) return Boolean;

   function Find (Cache : in Object_Cache;
                  Key   : in Object_Key) return Object_Record_Access;

   procedure Find (Cache  : in Object_Cache;
                   Object : in out Object_Ref'Class;
                   Key    : in Object_Key);

   --  Remove the object from the cache.  The reference counter associated
   --  with the object is decremented.
   --  Do nothing if the object is not in the cache.
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Record_Access);

   --  Remove the object from the cache.
   --  Do nothing if the object is not in the cache.
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Ref);

   --  Remove the object from the cache.
   --  Do nothing if the object is not in the cache.
   procedure Remove (Cache  : in out Object_Cache;
                     Object : in Object_Key);

   --  Remove all object in the cache.
   procedure Remove_All (Cache : in out Object_Cache);

private

   use Ada.Containers;

   --  The cache is a <b>Hashed_Set</b> in which the object record pointers
   --  are stored.  The hash and comparison methods are based on the object key
   --  and object type.
   function Hash (Item : Object_Record_Access) return Hash_Type;

   --  Compare whether the two objects pointed to by Left and Right have the same
   --  object key.
   function Equivalent_Elements (Left, Right : Object_Record_Access)
                               return Boolean;

   package Object_Set is new Indefinite_Hashed_Sets
     (Element_Type        => Object_Record_Access,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements);

   type Object_Cache is new Ada.Finalization.Limited_Controlled with record
      Objects : Object_Set.Set;
   end record;

   --  Finalize the object cache by removing all entries
   overriding
   procedure Finalize (Cache : in out Object_Cache);

end ADO.Objects.Cache;
