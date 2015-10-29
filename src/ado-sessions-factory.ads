-----------------------------------------------------------------------
--  factory -- Session Factory
--  Copyright (C) 2009, 2010, 2011, 2012, 2015 Stephane Carrez
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

with ADO.Databases;
with ADO.Schemas.Entities;
with ADO.Sequences;

--  === Session Factory ===
--  The session factory is the entry point to obtain a database session.
--  The <b>ADO.Sessions.Factory</b> package defines the factory for creating
--  sessions.
--
--    with ADO.Sessions.Factory;
--    ...
--    Sess_Factory : ADO.Sessions.Factory;
--
--  The session factory can be initialized by using the <tt>Create</tt> operation and
--  by giving a URI string that identifies the driver and the information to connect
--  to the database.  The session factory is created only once when the application starts.
--
--    ADO.Sessions.Factory.Create (Sess_Factory, "mysql://localhost:3306/ado_test?user=test");
--
--  Having a session factory, one can get a database by using the <tt>Get_Session</tt> or
--  <tt>Get_Master_Session</tt> function.  Each time this operation is called, a new session
--  is returned.  The session is released when the session variable is finalized.
--
--    DB : ADO.Sessions.Session := Sess_Factory.Get_Session;
--
package ADO.Sessions.Factory is

   pragma Elaborate_Body;

   --  ------------------------------
   --  Session factory
   --  ------------------------------
   type Session_Factory is tagged limited private;
   type Session_Factory_Access is access all Session_Factory'Class;

   --  Get a read-only session from the factory.
   function Get_Session (Factory : in Session_Factory) return Session;

   --  Get a read-write session from the factory.
   function Get_Master_Session (Factory : in Session_Factory) return Master_Session;

   --  Open a session
   procedure Open_Session (Factory : in out Session_Factory;
                           Database : out Session);

   --  Open a session
   procedure Open_Session (Factory : in Session_Factory;
                           Database : out Master_Session);

   --  Create the session factory to connect to the database represented
   --  by the data source.
   procedure Create (Factory : out Session_Factory;
                     Source  : in ADO.Databases.DataSource);

   --  Create the session factory to connect to the database identified
   --  by the URI.
   procedure Create (Factory : out Session_Factory;
                     URI     : in String);

   --  Get a read-only session from the session proxy.
   --  If the session has been invalidated, raise the SESSION_EXPIRED exception.
   function Get_Session (Proxy : in Session_Record_Access) return Session;
private

   --  The session factory holds the necessary information to obtain a master or slave
   --  database connection.  The sequence factory is shared by all sessions of the same
   --  factory (implementation is thread-safe).  The factory also contains the entity type
   --  cache which is initialized when the factory is created.
   type Session_Factory is tagged limited record
      Source       : ADO.Databases.DataSource;
      Sequences    : Factory_Access := null;
      Seq_Factory  : aliased ADO.Sequences.Factory;
      Entity_Cache : aliased ADO.Schemas.Entities.Entity_Cache;
      Entities     : ADO.Sessions.Entity_Cache_Access := null;
   end record;

   --  Initialize the sequence factory associated with the session factory.
   procedure Initialize_Sequences (Factory : in out Session_Factory);

end ADO.Sessions.Factory;
