-----------------------------------------------------------------------
--  ado-sessions-factory -- Session Factory
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Sequences;
with ADO.Caches;
with ADO.Audits;
with ADO.Sessions.Sources;

--  == Session Factory ==
--  The session factory is the entry point to obtain a database session.
--  The `ADO.Sessions.Factory` package defines the factory for creating
--  sessions.
--
--    with ADO.Sessions.Factory;
--    ...
--    Sess_Factory : ADO.Sessions.Factory;
--
--  The session factory can be initialized by using the `Create` operation and
--  by giving a URI string that identifies the driver and the information to connect
--  to the database.  The session factory is created only once when the application starts.
--
--    ADO.Sessions.Factory.Create (Sess_Factory, "mysql://localhost:3306/ado_test?user=test");
--
--  Having a session factory, one can get a database by using the `Get_Session` or
--  `Get_Master_Session` function.  Each time this operation is called, a new session
--  is returned.  The session is released when the session variable is finalized.
--
--    DB : ADO.Sessions.Session := Sess_Factory.Get_Session;
--
--  The session factory is also responsible for maintaining some data that is shared by
--  all the database connections.  This includes:
--
--    * the sequence generators used to allocate unique identifiers for database tables,
--    * the entity cache,
--    * some application specific global cache.
--
package ADO.Sessions.Factory is

   pragma Elaborate_Body;

   ENTITY_CACHE_NAME : constant String := "entity_type";

   --  ------------------------------
   --  Session factory
   --  ------------------------------
   type Session_Factory is tagged limited private;
   type Session_Factory_Access is access all Session_Factory'Class;

   --  Get a read-only session from the factory.
   function Get_Session (Factory : in Session_Factory) return Session;

   --  Get a read-write session from the factory.
   function Get_Master_Session (Factory : in Session_Factory) return Master_Session;

   --  Create the session factory to connect to the database represented
   --  by the data source.
   procedure Create (Factory : out Session_Factory;
                     Source  : in ADO.Sessions.Sources.Data_Source);

   --  Create the session factory to connect to the database identified
   --  by the URI.
   procedure Create (Factory : out Session_Factory;
                     URI     : in String);

   --  Get a read-only session from the session proxy.
   --  If the session has been invalidated, raise the Session_Error exception.
   function Get_Session (Proxy : in Session_Record_Access) return Session;

   --  Set the audit manager to be used for the object auditing support.
   procedure Set_Audit_Manager (Factory : in out Session_Factory;
                                Manager : in ADO.Audits.Audit_Manager_Access);

   --  Set a static query loader to load SQL queries.
   procedure Set_Query_Loader (Factory  : in out Session_Factory;
                               Loader   : in ADO.Queries.Static_Loader_Access);

   --  Set a generator to be used for the given sequence.
   procedure Set_Generator (Factory : in out Session_Factory;
                            Gen     : in ADO.Sequences.Generator_Access);
   procedure Set_Generator (Factory : in out Session_Factory;
                            Name    : in String;
                            Gen     : in String);

   --  Set the name of a sequence generator to be used by default when a generator is
   --  not found or was not configured by using `Set_Generator`.
   procedure Set_Default_Generator (Factory : in out Session_Factory;
                                    Name    : in String);

private

   --  The session factory holds the necessary information to obtain a master or slave
   --  database connection.  The sequence factory is shared by all sessions of the same
   --  factory (implementation is thread-safe).  The factory also contains the entity type
   --  cache which is initialized when the factory is created.
   type Session_Factory is tagged limited record
      Source       : ADO.Sessions.Sources.Data_Source;
      Sequences    : Factory_Access := null;
      Seq_Factory  : aliased ADO.Sequences.Factory;
      --  Entity_Cache : aliased ADO.Schemas.Entities.Entity_Cache;
      Entities     : ADO.Sessions.Entity_Cache_Access := null;
      Cache        : aliased ADO.Caches.Cache_Manager;
      Cache_Values : ADO.Caches.Cache_Manager_Access;
      Queries      : aliased ADO.Queries.Query_Manager;
      Audit        : ADO.Audits.Audit_Manager_Access;
   end record;

   --  Initialize the sequence factory associated with the session factory.
   procedure Initialize_Sequences (Factory : in out Session_Factory);

end ADO.Sessions.Factory;
