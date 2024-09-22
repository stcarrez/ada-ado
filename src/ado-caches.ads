-----------------------------------------------------------------------
--  ado-cache -- Simple cache management
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Finalization;

with ADO.Parameters;
with Util.Log.Loggers;

--  == Database Caches ==
--  The ADO cache manager allows to create and maintain cache of values and use the cache
--  from the SQL expander to replace cached values before evaluating the SQL.  The SQL expander
--  identifies constructs as follows:
--
--    $cache_name[entry-name]
--
--  and look for the cache identified by <tt>cache_name</tt> and then replace the cache entry
--  registered with the name <tt>entry-name</tt>.
--
--  The cache manager is represented by the <tt>Cache_Manager</tt> type and the database
--  session contains one cache manager.  Applications may use their own cache in that case
--  they will declare their cache as follows:
--
--     M : ADO.Caches.Cache_Manager;
--
--  A cache group is identified by a unique name and is represented by the <tt>Cache_Type</tt>
--  base class.  The cache group instance is registered in the cache manager by using the
--  <tt>Add_Cache</tt> operation.
package ADO.Caches is

   No_Value : exception;

   type Cache_Type is abstract limited new Ada.Finalization.Limited_Controlled with private;
   type Cache_Type_Access is access all Cache_Type'Class;

   --  Expand the name into a target parameter value to be used in the SQL query.
   --  The Expander can return a T_NULL when a value is not found or
   --  it may also raise some exception.
   function Expand (Instance : in out Cache_Type;
                    Name     : in String) return ADO.Parameters.Parameter is abstract;

   type Cache_Manager is limited new Ada.Finalization.Limited_Controlled
     and ADO.Parameters.Expander with private;
   type Cache_Manager_Access is access all Cache_Manager'Class;

   --  Expand the name from the given group into a target parameter value to be used in
   --  the SQL query.  The expander can look in a cache or in some configuration to find
   --  the value associated with the name and return it.  The Expander can return a
   --  T_NULL when a value is not found or it may also raise some exception.
   overriding
   function Expand (Instance : in Cache_Manager;
                    Group    : in String;
                    Name     : in String) return ADO.Parameters.Parameter;

   --  Insert a new cache in the manager.  The cache is identified by the given name.
   procedure Add_Cache (Manager : in out Cache_Manager;
                        Name    : in String;
                        Cache   : in Cache_Type_Access);

   --  Finalize the cache manager releasing every cache group.
   overriding
   procedure Finalize (Manager : in out Cache_Manager);

private

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Caches");

   type Cache_Type is abstract limited new Ada.Finalization.Limited_Controlled with record
      Next : Cache_Type_Access;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Cache_Manager is limited new Ada.Finalization.Limited_Controlled
     and ADO.Parameters.Expander with record
      First : Cache_Type_Access;
   end record;

end ADO.Caches;
