-----------------------------------------------------------------------
--  ado-schemas-entities -- Entity types cache
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Util.Strings;
with ADO.Model;
with ADO.Sessions;
package ADO.Schemas.Entities is

   No_Entity_Type : exception;

   --  The entity cache maintains a static cache of database entities.
   type Entity_Cache is private;

   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Table : in Class_Mapping_Access) return ADO.Model.Entity_Type_Ref;

   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Table : in Class_Mapping_Access) return ADO.Entity_Type;

   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Name  : in Util.Strings.Name_Access) return ADO.Entity_Type;

   --  Initialize the entity cache by reading the database entity table.
   procedure Initialize (Cache   : in out Entity_Cache;
                         Session : in out ADO.Sessions.Session'Class);

private

   package Entity_Map is new
     Ada.Containers.Hashed_Maps (Key_Type        => Util.Strings.Name_Access,
                                 Element_Type    => ADO.Model.Entity_Type_Ref,
                                 Hash            => Util.Strings.Hash,
                                 Equivalent_Keys => Util.Strings.Equivalent_Keys,
                                 "="             => ADO.Model."=");

   type Entity_Cache is record
      Entities : Entity_Map.Map;
   end record;

end ADO.Schemas.Entities;
