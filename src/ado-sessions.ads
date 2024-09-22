-----------------------------------------------------------------------
--  ado-sessions -- Sessions Management
--  Copyright (C) 2009 - 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;

with ADO.Configs;
with ADO.Schemas;
with ADO.Statements;
with ADO.Objects;
with ADO.Objects.Cache;
with ADO.Connections;
with ADO.Queries;
with ADO.SQL;
with ADO.Caches;

with Util.Concurrent.Counters;
limited with ADO.Sequences;
limited with ADO.Schemas.Entities;
limited with ADO.Audits;

--  = Session =
--  The `ADO.Sessions` package defines the control and management of database sessions.
--  The database session is represented by the `Session` or `Master_Session` types.
--  It provides operation to create a database statement that can be executed.
--  The `Session` type is used to represent read-only database sessions.  It provides
--  operations to query the database but it does not allow to update or delete content.
--  The `Master_Session` type extends the `Session` type to provide write
--  access and it provides operations to get update or delete statements.  The differentiation
--  between the two sessions is provided for the support of database replications with
--  databases such as MySQL.
--
--  @include ado-drivers.ads
--  @include ado-sessions-sources.ads
--  @include ado-sessions-factory.ads
--  @include ado-caches.ads
package ADO.Sessions is

   use ADO.Statements;

   --  Raised if the database connection is not open.
   Session_Error : exception;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception renames ADO.Configs.Connection_Error;

   --  The database connection status
   type Connection_Status is (OPEN, CLOSED);

   type Object_Factory is tagged private;

   --  ---------
   --  Session
   --  ---------
   --  Read-only database connection (or slave connection).
   --
   type Session is tagged private;

   --  Get the session status.
   function Get_Status (Database : in Session) return Connection_Status;

   --  Get the database driver which manages this connection.
   function Get_Driver (Database : in Session) return ADO.Connections.Driver_Access;

   --  Close the session.
   procedure Close (Database : in out Session);

   --  Insert a new cache in the manager.  The cache is identified by the given name.
   procedure Add_Cache (Database : in out Session;
                        Name     : in String;
                        Cache    : in ADO.Caches.Cache_Type_Access);

   --  Attach the object to the session.
   procedure Attach (Database : in out Session;
                     Object   : in ADO.Objects.Object_Ref'Class);

   --  Check if the session contains the object identified by the given key.
   function Contains (Database : in Session;
                      Item     : in ADO.Objects.Object_Key) return Boolean;

   --  Remove the object from the session cache.
   procedure Evict (Database : in out Session;
                    Item     : in ADO.Objects.Object_Key);

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement;

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Session;
                              Query    : in String)
                              return Query_Statement;

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Session;
                              Query    : in ADO.Queries.Context'Class)
                              return Query_Statement;

   --  Create a query statement and initialize the SQL statement with the query definition.
   function Create_Statement (Database : in Session;
                              Query    : in ADO.Queries.Query_Definition_Access)
                              return Query_Statement;

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Session;
                              Query    : in ADO.SQL.Query'Class;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement;

   --  Execute the SQL statement.
   procedure Execute (Database : in Session'Class;
                      SQL      : in String);

   --  Load the database schema definition for the current database.
   procedure Load_Schema (Database : in Session;
                          Schema   : out ADO.Schemas.Schema_Definition);

   --  Check if the table with the given name exists in the database.
   function Has_Table (Database : in Session;
                       Name     : in String) return Boolean;

   --  Internal method to get the session proxy associated with the given database session.
   --  The session proxy is associated with some ADO objects to be able to retrieve the database
   --  session for the implementation of lazy loading.  The session proxy is kept until the
   --  session exist and at least one ADO object is refering to it.
   function Get_Session_Proxy (Database : in Session) return ADO.Objects.Session_Proxy_Access;

   --  ---------
   --  Master Session
   --  ---------
   --  Read-write session.
   --
   type Master_Session is new Session with private;

   --  Start a transaction.
   procedure Begin_Transaction (Database : in out Master_Session);

   --  Commit the current transaction.
   procedure Commit (Database : in out Master_Session);

   --  Rollback the current transaction.
   procedure Rollback (Database : in out Master_Session);

   --  Allocate an identifier for the table.
   procedure Allocate (Database : in out Master_Session;
                       Id       : in out ADO.Objects.Object_Record'Class);

   --  Flush the objects that were modified.
   procedure Flush (Database : in out Master_Session);

   --  Create a delete statement
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement;

   --  Create an update statement
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement;

   --  Create an insert statement
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement;

   --  Get the audit manager.
   function Get_Audit_Manager (Database : in Master_Session)
                               return access Audits.Audit_Manager'Class;

   subtype Database_Connection is Connections.Database_Connection;

   --  Internal operation to get access to the database connection.
   procedure Access_Connection (Database : in out Master_Session;
                                Process  : not null access
                                  procedure (Connection : in out Database_Connection'Class));

   type Session_Record is limited private;
   type Session_Record_Access is access all Session_Record;

private

   type Entity_Cache_Access is access ADO.Schemas.Entities.Entity_Cache;

   type Object_Factory is tagged record
      A : Integer;
   end record;
   type Object_Factory_Access is access all Object_Factory'Class;

   --  The <b>Session_Record</b> maintains the connection information to the database for
   --  the duration of the session.  It also maintains a cache of application objects
   --  which is used when session objects are fetched (either through Load or a Find).
   --  The connection is released and the session record is deleted when the session
   --  is closed with <b>Close</b>.
   --
   --  For the lazy loading support, each object is associated with a shared <b>Session_Proxy</b>
   --  object that allows to give access to the session record associated with the object.
   --  When a session is closed, the <b>Session_Proxy</b> is not deleted but it is simply
   --  unlinked from the session record.
   type Session_Record is limited record
      Counter  : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
      Database : ADO.Connections.Ref.Ref;
      Proxy    : ADO.Objects.Session_Proxy_Access;
      Cache    : ADO.Objects.Cache.Object_Cache;
      Entities : Entity_Cache_Access;
      Values   : ADO.Caches.Cache_Manager_Access;
      Queries  : ADO.Queries.Query_Manager_Access;
   end record;

   type Session is new Ada.Finalization.Controlled with record
      Impl : Session_Record_Access := null;
   end record;

   overriding
   procedure Adjust (Object : in out Session);

   overriding
   procedure Finalize (Object : in out Session);

   type Factory_Access is access all ADO.Sequences.Factory;

   type Master_Session is new Session with record
      Sequences : Factory_Access;
      Audit     : access ADO.Audits.Audit_Manager'Class;
   end record;

   procedure Check_Session (Database : in Session'Class;
                            Message  : in String := "");
   pragma Inline (Check_Session);

end ADO.Sessions;
