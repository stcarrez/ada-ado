-----------------------------------------------------------------------
--  ado-schemas-entities -- Entity types cache
--  Copyright (C) 2011, 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with ADO.Sessions;
with ADO.Parameters;
with ADO.Caches;
package ADO.Schemas.Entities is

   No_Entity_Type : exception;

   --  The entity cache maintains a static cache of database entities.
   type Entity_Cache is new ADO.Caches.Cache_Type with private;

   --  Expand the name into a target parameter value to be used in the SQL query.
   --  The Expander can return a T_NULL when a value is not found or
   --  it may also raise some exception.
   overriding
   function Expand (Instance : in out Entity_Cache;
                    Name     : in String) return ADO.Parameters.Parameter;

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
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                            Element_Type    => ADO.Entity_Type,
                                            Hash            => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   type Entity_Cache is new ADO.Caches.Cache_Type with record
      Entities : Entity_Map.Map;
   end record;

end ADO.Schemas.Entities;
