-----------------------------------------------------------------------
--  ado-sessions-entities -- Find entity types
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Strings;

with ADO.Model;
with ADO.Schemas;
with ADO.Objects;
with ADO.Parameters;
package ADO.Sessions.Entities is

   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Object  : in ADO.Objects.Object_Key)
                              return ADO.Model.Entity_Type_Ref;

   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Table   : in ADO.Schemas.Class_Mapping_Access)
                              return ADO.Model.Entity_Type_Ref;

   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Table   : in ADO.Schemas.Class_Mapping_Access)
                              return ADO.Entity_Type;

   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Name    : in Util.Strings.Name_Access)
                              return ADO.Entity_Type;

   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Name    : in String) return ADO.Entity_Type;

   --  Resolve the entity type associated with the database table <b>Table</b> by using
   --  the <b<Session</b> connection object.  Then, bind the parameter identified by
   --  <b>Name</b> in the parameter list.
   procedure Bind_Param (Params  : in out ADO.Parameters.Abstract_List'Class;
                         Name    : in String;
                         Table   : in ADO.Schemas.Class_Mapping_Access;
                         Session : in ADO.Sessions.Session'Class);

end ADO.Sessions.Entities;
