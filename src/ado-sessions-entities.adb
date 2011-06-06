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

with ADO.Schemas.Entities;
package body ADO.Sessions.Entities is

   --  ------------------------------
   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Table   : in ADO.Schemas.Class_Mapping_Access)
                              return ADO.Model.Entity_Type_Ref is
   begin
      Check_Session (Session);
      return ADO.Schemas.Entities.Find_Entity_Type (Session.Impl.Entities.all, Table);
   end Find_Entity_Type;

   --  ------------------------------
   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Table   : in ADO.Schemas.Class_Mapping_Access)
                              return ADO.Entity_Type is
   begin
      Check_Session (Session);
      return ADO.Schemas.Entities.Find_Entity_Type (Session.Impl.Entities.all, Table);
   end Find_Entity_Type;

end ADO.Sessions.Entities;
