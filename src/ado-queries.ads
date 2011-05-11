-----------------------------------------------------------------------
--  ado-queries -- Database Queries
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

with Util.Strings;
with Util.Refs;
with ADO.SQL;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
package ADO.Queries is

   type Query_Definition_Record is limited record
      Name : Util.Strings.Name_Access;
   end record;

   type Query_Definition is access all Query_Definition_Record;

   type Context is new ADO.SQL.Query with null record;

   type Query_Info is new Util.Refs.Ref_Entity with private;

   procedure Read_Query (Into : in out Query_Info;
                         Path : in String);

private

   use Ada.Strings.Unbounded;

   type Query_Info is new Util.Refs.Ref_Entity with record
      Query : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Query_Info_Access is access all Query_Info;

   package Query_Info_Ref is
      new Util.Refs.References (Query_Info, Query_Info_Access);

   package Query_Info_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                     Element_Type    => Query_Info_Ref.Ref,
                                     Hash            => Hash,
                                     Equivalent_Keys => "=",
                                     "="             => Query_Info_Ref."=");

end ADO.Queries;
