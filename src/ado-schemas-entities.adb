-----------------------------------------------------------------------
--  ado-schemas-entities -- Entity types cache
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

with Util.Log.Loggers;

with ADO.SQL;
package body ADO.Schemas.Entities is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Schemas.Entities");

   --  ------------------------------
   --  Find the entity type object associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Table : in Class_Mapping_Access) return ADO.Model.Entity_Type_Ref is
      Pos : constant Entity_Map.Cursor := Cache.Entities.Find (Table.Table);
   begin
      if not Entity_Map.Has_Element (Pos) then
         Log.Error ("No entity type associated with table {0}", Table.Table.all);
         raise No_Entity_Type with "No entity type associated with table " & Table.Table.all;
      end if;
      return Entity_Map.Element (Pos);
   end Find_Entity_Type;

   --  ------------------------------
   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Table : in Class_Mapping_Access) return ADO.Entity_Type is
   begin
      return Find_Entity_Type (Cache, Table.Table);
   end Find_Entity_Type;

   --  ------------------------------
   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Name  : in Util.Strings.Name_Access) return ADO.Entity_Type is
      Pos : constant Entity_Map.Cursor := Cache.Entities.Find (Name);
   begin
      if not Entity_Map.Has_Element (Pos) then
         Log.Error ("No entity type associated with table {0}", Name.all);
         raise No_Entity_Type with "No entity type associated with table " & Name.all;
      end if;
      return Entity_Type (Entity_Map.Element (Pos).Get_Id);
   end Find_Entity_Type;

   --  ------------------------------
   --  Initialize the entity cache by reading the database entity table.
   --  ------------------------------
   procedure Initialize (Cache   : in out Entity_Cache;
                         Session : in out ADO.Sessions.Session'Class) is
      use type Ada.Containers.Count_Type;
      List  : ADO.Model.Entity_Type_Vector;
      Query : ADO.SQL.Query;

      procedure Process (Element : in ADO.Model.Entity_Type_Ref) is
         Name : constant Util.Strings.Name_Access := new String'(Element.Get_Name);
      begin
         Cache.Entities.Insert (Key => Name, New_Item => Element);
      end Process;

   begin
      ADO.Model.List (List, Session, Query);
      for I in 0 .. List.Length - 1 loop
         List.Query_Element (Natural (I), Process'Access);
      end loop;
   end Initialize;

end ADO.Schemas.Entities;
