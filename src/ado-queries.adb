-----------------------------------------------------------------------
--  ado-queries -- Database Queries
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2017 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with ADO.Queries.Loaders;
package body ADO.Queries is

   --  ------------------------------
   --  Set the query definition which identifies the SQL query to execute.
   --  The query is represented by the <tt>sql</tt> XML entry.
   --  ------------------------------
   procedure Set_Query (Into  : in out Context;
                        Query : in Query_Definition_Access) is
   begin
      Into.Query_Def := Query;
      Into.Is_Count  := False;
   end Set_Query;

   --  ------------------------------
   --  Set the query definition which identifies the SQL query to execute.
   --  The query count is represented by the <tt>sql-count</tt> XML entry.
   --  ------------------------------
   procedure Set_Count_Query (Into  : in out Context;
                              Query : in Query_Definition_Access) is
   begin
      Into.Query_Def := Query;
      Into.Is_Count  := True;
   end Set_Count_Query;

   procedure Set_Query (Into  : in out Context;
                        Name  : in String) is
   begin
      Into.Query_Def := ADO.Queries.Loaders.Find_Query (Name);
   end Set_Query;

   --  ------------------------------
   --  Set the query to execute as SQL statement.
   --  ------------------------------
   procedure Set_SQL (Into : in out Context;
                      SQL  : in String) is
   begin
      ADO.SQL.Clear (Into.SQL);
      ADO.SQL.Append (Into.SQL, SQL);
   end Set_SQL;

   --  ------------------------------
   --  Set the limit for the SQL query.
   --  ------------------------------
   procedure Set_Limit (Into  : in out Context;
                        First : in Natural;
                        Last  : in Natural) is
   begin
      Into.First := First;
      Into.Last  := Last;
   end Set_Limit;

   --  ------------------------------
   --  Get the first row index.
   --  ------------------------------
   function Get_First_Row_Index (From : in Context) return Natural is
   begin
      return From.First;
   end Get_First_Row_Index;

   --  ------------------------------
   --  Get the last row index.
   --  ------------------------------
   function Get_Last_Row_Index (From : in Context) return Natural is
   begin
      return From.Last;
   end Get_Last_Row_Index;

   --  ------------------------------
   --  Get the maximum number of rows that the SQL query can return.
   --  This operation uses the <b>sql-count</b> query.
   --  ------------------------------
   function Get_Max_Row_Count (From : in Context) return Natural is
   begin
      return From.Max_Row_Count;
   end Get_Max_Row_Count;

   --  ------------------------------
   --  Get the SQL query that correspond to the query context.
   --  ------------------------------
   function Get_SQL (From   : in Context;
                     Manager : in Query_Manager'Class) return String is
   begin
      if From.Query_Def = null then
         return ADO.SQL.To_String (From.SQL);
      else
         return Get_SQL (From.Query_Def, Manager, From.Is_Count);
      end if;
   end Get_SQL;

   --  ------------------------------
   --  Find the query with the given name.
   --  Returns the query definition that matches the name or null if there is none
   --  ------------------------------
   function Find_Query (File : in Query_File_Info;
                        Name : in String) return Query_Definition_Access is
      Query : Query_Definition_Access := File.File.Queries;
   begin
      while Query /= null loop
         if Query.Name.all = Name then
            return Query;
         end if;
         Query := Query.Next;
      end loop;
      return null;
   end Find_Query;

   function Get_SQL (From      : in Query_Definition_Access;
                     Manager   : in Query_Manager;
                     Use_Count : in Boolean) return String is
      Query : Query_Info_Ref.Ref;
   begin
      ADO.Queries.Loaders.Read_Query (Manager, From);
      Query := Manager.Queries (From.Query).Get;
      if Query.Is_Null then
         return "";
      end if;
      if Use_Count then
         if Length (Query.Value.Count_Query (Manager.Driver).SQL) > 0 then
            return To_String (Query.Value.Count_Query (Manager.Driver).SQL);
         else
            return To_String (Query.Value.Count_Query (ADO.Drivers.Driver_Index'First).SQL);
         end if;
      elsif Length (Query.Value.Main_Query (Manager.Driver).SQL) > 0 then
         return To_String (Query.Value.Main_Query (Manager.Driver).SQL);
      else
         return To_String (Query.Value.Main_Query (ADO.Drivers.Driver_Index'First).SQL);
      end if;
   end Get_SQL;

   overriding
   procedure Finalize (Manager : in out Query_Manager) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => File_Table,
                                        Name   => File_Table_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Query_Table,
                                        Name   => Query_Table_Access);
   begin
      Free (Manager.Queries);
      Free (Manager.Files);
   end Finalize;

end ADO.Queries;
