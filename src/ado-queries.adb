-----------------------------------------------------------------------
--  ado-queries -- Database Queries
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
with Util.Refs;
with ADO.SQL;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Util.Beans.Objects;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
package body ADO.Queries is

   type Query_Info_Fields is (FIELD_SQL, FIELD_SQL_COUNT);

   procedure Set_Member (Into  : in out Query_Info;
                         Field : in Query_Info_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_SQL =>
            Into.Query := Util.Beans.Objects.To_Unbounded_String (Value);

         when FIELD_SQL_COUNT =>
            null;

      end case;
   end Set_Member;

   package Query_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Query_Info,
                                               Element_Type_Access => Query_Info_Access,
                                               Fields              => Query_Info_Fields,
                                               Set_Member          => Set_Member);

   procedure Read_Query (Into : in out Query_Info;
                         Path : in String) is

      Sql_Mapper       : aliased Query_Mapper.Mapper;
      Sql_Count_Mapper : aliased Query_Mapper.Mapper;
      Reader           : Util.Serialize.IO.XML.Parser;
   begin

      Sql_Mapper.Add_Mapping ("sql", FIELD_SQL);
      Sql_Mapper.Add_Mapping ("sql-count", FIELD_SQL_COUNT);
      Reader.Add_Mapping ("query", Sql_Mapper'Unchecked_Access);
      Query_Mapper.Set_Context (Reader, Into'Unchecked_Access);
      Reader.Parse (Path);
   end Read_Query;


end ADO.Queries;
