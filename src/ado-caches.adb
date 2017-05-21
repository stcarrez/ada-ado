-----------------------------------------------------------------------
--  ado-cache -- Simple cache management
--  Copyright (C) 2017 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
package body ADO.Caches is

   --  --------------------
   --  Expand the name from the given group into a target parameter value to be used in
   --  the SQL query.  The expander can look in a cache or in some configuration to find
   --  the value associated with the name and return it.  The Expander can return a
   --  T_NULL when a value is not found or it may also raise some exception.
   --  --------------------
   overriding
   function Expand (Instance : in Cache_Manager;
                    Group    : in String;
                    Name     : in String) return ADO.Parameters.Parameter is
      use type Ada.Strings.Unbounded.Unbounded_String;
      C : Cache_Type_Access := Instance.First;
   begin
      while C /= null loop
         if C.Name = Group then
            return C.Expand (Name);
         end if;
         C := C.Next;
      end loop;
      Log.Warn ("There is no group {0} registered in the cache manager", Group);
      raise No_Value with "No group '" & Group & "'";
   end Expand;

   --  --------------------
   --  Insert a new cache in the manager.  The cache is identified by the given name.
   --  --------------------
   procedure Add_Cache (Manager : in out Cache_Manager;
                        Name    : in String;
                        Cache   : in Cache_Type_Access) is
   begin
      Log.Debug ("Adding cache group {0}", Name);

      Cache.Next    := Manager.First;
      Cache.Name    := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Manager.First := Cache;
   end Add_Cache;

   --  --------------------
   --  Finalize the cache manager releasing every cache group.
   --  --------------------
   overriding
   procedure Finalize (Manager : in out Cache_Manager) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Cache_Type'Class,
                                        Name   => Cache_Type_Access);
      Cache : Cache_Type_Access;
   begin
      loop
         Cache := Manager.First;
         exit when Cache = null;
         Manager.First := Cache.Next;
         Free (Cache);
      end loop;
   end Finalize;

end ADO.Caches;
