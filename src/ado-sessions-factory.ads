-----------------------------------------------------------------------
--  factory -- Session Factory
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with ADO.Databases;
with ADO.Sequences;
with ADO.Schemas.Entities;

--  The <b>ADO.Sessions.Factory</b> package defines the factory for creating
--  sessions.
package ADO.Sessions.Factory is

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
   function Get_Session (Proxy : in Session_Proxy_Access) return Session;

   function Get_Session (Proxy : in Session_Record_Access) return Session;
private

   --  The session factory holds the necessary information to obtain a master or slave
   --  database connection.  The sequence factory is shared by all sessions of the same
   --  factory (implementation is thread-safe).  The factory also contains the entity type
   --  cache which is initialized when the factory is created.
   type Session_Factory is new Ada.Finalization.Limited_Controlled with record
      Source       : ADO.Databases.DataSource;
      Sequences    : access ADO.Sequences.Factory;
      Entity_Cache : aliased ADO.Schemas.Entities.Entity_Cache;
      Entities     : ADO.Sessions.Entity_Cache_Access;
   end record;

   --  Finalize and release the factory
   overriding
   procedure Finalize (Factory : in out Session_Factory);

   --  Initialize the sequence factory associated with the session factory.
   procedure Initialize_Sequences (Factory : in out Session_Factory);

end ADO.Sessions.Factory;
