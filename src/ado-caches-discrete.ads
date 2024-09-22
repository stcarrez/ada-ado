-----------------------------------------------------------------------
--  ado-cache-discrete -- Simple cache management for discrete types
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with ADO.Sessions;

generic
   type Element_Type is (<>);
package ADO.Caches.Discrete is
   pragma Elaborate_Body;

   --  The cache type that maintains a cache of name/value pairs.
   type Cache_Type is new ADO.Caches.Cache_Type with private;
   type Cache_Type_Access is access all Cache_Type'Class;

   --  Expand the name into a target parameter value to be used in the SQL query.
   --  The Expander can return a T_NULL when a value is not found or
   --  it may also raise some exception.
   overriding
   function Expand (Instance : in out Cache_Type;
                    Name     : in String) return ADO.Parameters.Parameter;

   --  Find the value associated with the given name.
   --  Raises the No_Value exception if no such mapping exist.
   function Find (Cache : in out Cache_Type;
                  Name  : in String) return Element_Type;

   --  Insert the value associated with the given name in the cache.
   --  When <tt>Override</tt> is set, override existing values otherwise raise an exception.
   procedure Insert (Cache    : in out Cache_Type;
                     Name     : in String;
                     Value    : in Element_Type;
                     Override : in Boolean := False);

   --  Delete the value associated with the given name in the cache.
   --  Raise the No_Value exception if the value is not found and <tt>Ignore</tt> is not set.
   procedure Delete (Cache  : in out Cache_Type;
                     Name   : in String;
                     Ignore : in Boolean := False);

   --  Initialize the entity cache by reading the database entity table.
   procedure Initialize (Cache   : in out Cache_Type;
                         Session : in out ADO.Sessions.Session'Class);

private

   package Cache_Map is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                            Element_Type    => Element_Type,
                                            Hash            => Ada.Strings.Hash,
                                            Equivalent_Keys => "=");

   protected type Cache_Controller is

      --  Find the value associated with the given name.
      --  Raises the No_Value exception if no such mapping exist.
      function Find (Name  : in String) return Element_Type;

      --  Insert the value associated with the given name in the cache.
      --  When <tt>Override</tt> is set, override existing values otherwise raise an exception.
      procedure Insert (Name     : in String;
                        Value    : in Element_Type;
                        Override : in Boolean := False);

      --  Delete the value associated with the given name in the cache.
      --  Raise the No_Value exception if the value is not found and <tt>Ignore</tt> is not set.
      procedure Delete (Name   : in String;
                        Ignore : in Boolean := False);

   private
      Values : Cache_Map.Map;
   end Cache_Controller;

   type Cache_Type is new ADO.Caches.Cache_Type with record
      Controller : Cache_Controller;
   end record;

end ADO.Caches.Discrete;
