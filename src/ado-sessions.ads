-----------------------------------------------------------------------
--  ADO Sessions -- Sessions Management
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with ADO.Schemas;
with ADO.Statements;
with ADO.Objects;
with ADO.Objects.Cache;
with ADO.Databases;
with ADO.Queries;
with ADO.SQL;

with Util.Concurrent.Counters;
limited with ADO.Sequences;
limited with ADO.Schemas.Entities;

package ADO.Sessions is

   use ADO.Statements;

   --  Raised for all errors reported by the database
   DB_Error : exception;

   --  Raised if the database connection is not open.
   NOT_OPEN : exception;

   NOT_FOUND : exception;

   NO_DATABASE : exception;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception;

   type Object_Factory is tagged private;

   --  ---------
   --  Session
   --  ---------
   --  Read-only database connection (or slave connection).
   --
   type Session is tagged private;

   --  Get the session status.
   function Get_Status (Database : in Session) return ADO.Databases.Connection_Status;

   --  Close the session.
   procedure Close (Database : in out Session);

   --  Get the database connection.
   function Get_Connection (Database : in Session) return ADO.Databases.Connection'Class;

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

   type Session_Proxy is limited private;
   type Session_Proxy_Access is access all Session_Proxy;

   function Get_Session_Proxy (Database : in Session) return ADO.Objects.Session_Proxy_Access;

   type Session_Record is limited private;
   type Session_Record_Access is access all Session_Record;

private

   type Entity_Cache_Access is access constant ADO.Schemas.Entities.Entity_Cache;

   type Object_Factory is tagged record
      A : Integer;
   end record;
   type Object_Factory_Access is access all Object_Factory'Class;

   type Session_Proxy is limited record
      Counter : Util.Concurrent.Counters.Counter;
      Session : Session_Record_Access;
      Factory : Object_Factory_Access;
   end record;

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
      Counter  : Natural := 1;
      Database : ADO.Databases.Master_Connection;
      Proxy    : ADO.Objects.Session_Proxy_Access;
      Cache    : ADO.Objects.Cache.Object_Cache;
      Entities : Entity_Cache_Access;
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
   end record;

   procedure Check_Session (Database : in Session'Class);
   pragma Inline (Check_Session);

end ADO.Sessions;
