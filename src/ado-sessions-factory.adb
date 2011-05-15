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

with ADO.Sequences.Hilo;
with Ada.Unchecked_Deallocation;

--  The <b>ADO.Sessions.Factory</b> package defines the factory for creating
--  sessions.
package body ADO.Sessions.Factory is

   use ADO.Databases;

   --  ------------------------------
   --  Open a session
   --  ------------------------------
   procedure Open_Session (Factory  : in out Session_Factory;
                           Database : out Session) is
      S  : constant Session_Record_Access := new Session_Record;
      DB : constant Master_Connection'Class := Factory.Source.Get_Connection;
   begin
      S.Database := Master_Connection (DB);
      Database.Impl := S;
   end Open_Session;

   --  ------------------------------
   --  Get a read-only session from the factory.
   --  ------------------------------
   function Get_Session (Factory : in Session_Factory) return Session is
      R  : Session;
      S  : constant Session_Record_Access := new Session_Record;
      DB : constant Master_Connection'Class := Factory.Source.Get_Connection;
   begin
      S.Database := Master_Connection (DB);
      R.Impl := S;
      return R;
   end Get_Session;

   --  ------------------------------
   --  Get a read-only session from the session proxy.
   --  If the session has been invalidated, raise the SESSION_EXPIRED exception.
   --  ------------------------------
   function Get_Session (Proxy : in Session_Proxy_Access) return Session is
      R : Session;
   begin
      if Proxy = null then
         raise ADO.Objects.SESSION_EXPIRED;
      end if;
      if Proxy.Session = null then
         raise ADO.Objects.SESSION_EXPIRED;
      end if;
      R.Impl := Proxy.Session;
      R.Impl.Counter := R.Impl.Counter + 1;
      return R;
   end Get_Session;

   --  ------------------------------
   --  Get a read-only session from the session proxy.
   --  If the session has been invalidated, raise the SESSION_EXPIRED exception.
   --  ------------------------------
   function Get_Session (Proxy : in Session_Record_Access) return Session is
      R : Session;
   begin
      if Proxy = null then
         raise ADO.Objects.SESSION_EXPIRED;
      end if;
--        if Proxy.Session = null then
--           raise ADO.Objects.SESSION_EXPIRED;
--        end if;
      R.Impl := Proxy;
      R.Impl.Counter := R.Impl.Counter + 1;
      return R;
   end Get_Session;

   --  ------------------------------
   --  Get a read-write session from the factory.
   --  ------------------------------
   function Get_Master_Session (Factory : in Session_Factory) return Master_Session is
      R  : Master_Session;
      S  : constant Session_Record_Access   := new Session_Record;
      DB : constant Master_Connection'Class := Factory.Source.Get_Connection;
   begin
      S.Database := Master_Connection (DB);
      R.Impl := S;
      R.Sequences := Factory.Sequences;
      return R;
   end Get_Master_Session;

   --  ------------------------------
   --  Open a session
   --  ------------------------------
   procedure Open_Session (Factory : in Session_Factory;
                           Database : out Master_Session) is
   begin
      null;
   end Open_Session;

   --  ------------------------------
   --  Initialize the sequence factory associated with the session factory.
   --  ------------------------------
   procedure Initialize_Sequences (Factory : in out Session_Factory) is
      use ADO.Sequences;
   begin
      Factory.Sequences := new ADO.Sequences.Factory;
      Set_Default_Generator (Factory.Sequences.all,
                             ADO.Sequences.Hilo.Create_HiLo_Generator'Access,
                             Factory'Unrestricted_Access);
   end Initialize_Sequences;

   --  ------------------------------
   --  Create the session factory to connect to the database represented
   --  by the data source.
   --  ------------------------------
   procedure Create (Factory : out Session_Factory;
                     Source  : in ADO.Databases.DataSource) is
   begin
      Factory.Source := Source;
      Initialize_Sequences (Factory);
   end Create;

   --  ------------------------------
   --  Create the session factory to connect to the database identified
   --  by the URI.
   --  ------------------------------
   procedure Create (Factory : out Session_Factory;
                     URI     : in String) is
   begin
      Factory.Source.Set_Connection (URI);
      Initialize_Sequences (Factory);
   end Create;

   --  ------------------------------
   --  Finalize and release the factory
   --  ------------------------------
   overriding
   procedure Finalize (Factory : in out Session_Factory) is

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => ADO.Sequences.Factory,
                                    Name   => ADO.Sequences.Factory_Access);

      Seq : ADO.Sequences.Factory_Access;
   begin
      if Factory.Sequences /= null then
         Seq := Factory.Sequences.all'Access;
         Free (Seq);
         Factory.Sequences := null;
      end if;
   end Finalize;

end ADO.Sessions.Factory;
