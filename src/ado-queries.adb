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

with ADO.Queries.Loaders;
package body ADO.Queries is

   --  ------------------------------
   --  Find the query with the given name.
   --  Returns the query definition that matches the name or null if there is none
   --  ------------------------------
   function Find_Query (File : in Query_File;
                        Name : in String) return Query_Definition_Access is
      Query : Query_Definition_Access := File.Queries;
   begin
      while Query /= null loop
         if Query.Name.all = Name then
            return Query;
         end if;
         Query := Query.Next;
      end loop;
      return null;
   end Find_Query;

   function Get_SQL (From   : in Query_Definition_Access;
                     Driver : in ADO.Drivers.Driver_Index) return String is
   begin
      ADO.Queries.Loaders.Read_Query (From);
      if From.Query = null then
         return "";
      end if;
      if Length (From.Query.Main_Query (Driver).SQL) > 0 then
         return To_String (From.Query.Main_Query (Driver).SQL);
      else
         return To_String (From.Query.Main_Query (ADO.Drivers.Driver_Index'First).SQL);
      end if;
   end Get_SQL;

end ADO.Queries;
