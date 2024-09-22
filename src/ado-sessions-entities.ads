-----------------------------------------------------------------------
--  ado-sessions-entities -- Find entity types
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings;

with ADO.Schemas;
with ADO.Objects;
with ADO.Parameters;
package ADO.Sessions.Entities is

   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   function Find_Entity_Type (Session : in ADO.Sessions.Session'Class;
                              Object  : in ADO.Objects.Object_Key)
                                   return ADO.Entity_Type;

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
