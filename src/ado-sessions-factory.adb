-----------------------------------------------------------------------
--  ado-sessions-factory -- Session Factory
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2022 Stephane Carrez
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
with ADO.Schemas.Entities;
with ADO.Queries.Loaders;

package body ADO.Sessions.Factory is

   --  ------------------------------
   --  Get a read-only session from the factory.
   --  ------------------------------
   function Get_Session (Factory : in Session_Factory) return Session is
      R  : Session;
      S  : constant Session_Record_Access := new Session_Record;
   begin
      R.Impl := S;
      S.Entities := Factory.Entities;
      S.Values   := Factory.Cache_Values;
      S.Queries  := Factory.Queries'Unrestricted_Access;
      Factory.Source.Create_Connection (S.Database);
      return R;
   end Get_Session;

   --  ------------------------------
   --  Get a read-only session from the session proxy.
   --  If the session has been invalidated, raise the Session_Error exception.
   --  ------------------------------
   function Get_Session (Proxy : in Session_Record_Access) return Session is
      R : Session;
   begin
      if Proxy = null then
         raise Session_Error;
      end if;
      R.Impl := Proxy;
      Util.Concurrent.Counters.Increment (R.Impl.Counter);
      return R;
   end Get_Session;

   --  ------------------------------
   --  Get a read-write session from the factory.
   --  ------------------------------
   function Get_Master_Session (Factory : in Session_Factory) return Master_Session is
      R  : Master_Session;
      S  : constant Session_Record_Access   := new Session_Record;
   begin
      R.Impl := S;
      R.Sequences := Factory.Sequences;
      R.Audit := Factory.Audit;
      S.Entities := Factory.Entities;
      S.Values   := Factory.Cache_Values;
      S.Queries  := Factory.Queries'Unrestricted_Access;
      Factory.Source.Create_Connection (S.Database);
      return R;
   end Get_Master_Session;

   --  ------------------------------
   --  Initialize the sequence factory associated with the session factory.
   --  ------------------------------
   procedure Initialize_Sequences (Factory : in out Session_Factory) is
      use ADO.Sequences;
   begin
      Factory.Sequences := Factory.Seq_Factory'Unchecked_Access;
      Set_Default_Generator (Factory.Seq_Factory,
                             ADO.Sequences.Hilo.Create_HiLo_Generator'Access,
                             Factory'Unchecked_Access);
   end Initialize_Sequences;

   --  ------------------------------
   --  Create the session factory to connect to the database represented
   --  by the data source.
   --  ------------------------------
   procedure Create (Factory : out Session_Factory;
                     Source  : in ADO.Sessions.Sources.Data_Source) is
   begin
      Factory.Source := Source;
      Factory.Entities := new ADO.Schemas.Entities.Entity_Cache;
      Factory.Cache_Values := Factory.Cache'Unchecked_Access;
      Factory.Cache.Add_Cache (ENTITY_CACHE_NAME, Factory.Entities.all'Access);
      ADO.Queries.Loaders.Initialize (Factory.Queries, Factory.Source);
      Initialize_Sequences (Factory);

      if Factory.Source.Get_Database /= ""
        and then not Configs.Is_On (Configs.NO_ENTITY_LOAD)
      then
         declare
            S : Session := Factory.Get_Session;
         begin
            ADO.Schemas.Entities.Initialize (Factory.Entities.all, S);
         end;
      end if;
   end Create;

   --  ------------------------------
   --  Create the session factory to connect to the database identified
   --  by the URI.
   --  ------------------------------
   procedure Create (Factory : out Session_Factory;
                     URI     : in String) is
   begin
      Factory.Source.Set_Connection (URI);
      Factory.Entities := new ADO.Schemas.Entities.Entity_Cache;
      Factory.Cache_Values := Factory.Cache'Unchecked_Access;
      Factory.Cache.Add_Cache (ENTITY_CACHE_NAME, Factory.Entities.all'Access);
      ADO.Queries.Loaders.Initialize (Factory.Queries, Factory.Source);
      Initialize_Sequences (Factory);

      if Factory.Source.Get_Database /= ""
        and then not Configs.Is_On (Configs.NO_ENTITY_LOAD)
      then
         declare
            S : Session := Factory.Get_Session;
         begin
            ADO.Schemas.Entities.Initialize (Factory.Entities.all, S);
         end;
      end if;
   end Create;

   --  ------------------------------
   --  Set the audit manager to be used for the object auditing support.
   --  ------------------------------
   procedure Set_Audit_Manager (Factory : in out Session_Factory;
                                Manager : in ADO.Audits.Audit_Manager_Access) is
   begin
      Factory.Audit := Manager;
   end Set_Audit_Manager;

   --  ------------------------------
   --  Set a static query loader to load SQL queries.
   --  ------------------------------
   procedure Set_Query_Loader (Factory  : in out Session_Factory;
                               Loader   : in ADO.Queries.Static_Loader_Access) is
   begin
      Factory.Queries.Set_Query_Loader (Loader);
   end Set_Query_Loader;

end ADO.Sessions.Factory;
