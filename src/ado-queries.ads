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
with ADO.Drivers;

with Interfaces;
with Ada.Strings.Unbounded;
package ADO.Queries is

   type Query_File;
   type Query_File_Access is access all Query_File;

   type Query_Definition;
   type Query_Definition_Access is access all Query_Definition;

   type Query_Info is limited private;
   type Query_Info_Access is access all Query_Info;

   --  ------------------------------
   --  Query Context
   --  ------------------------------
   --  The <b>Context</b> type holds the necessary information to build and execute
   --  a query whose SQL pattern is defined in an XML query file.
   type Context is new ADO.SQL.Query with private;

   --  Set the query definition which identifies the SQL query to execute.
   procedure Set_Query (Into  : in out Context;
                        Query : in Query_Definition_Access);

   --  Set the limit for the SQL query.
   procedure Set_Limit (Into  : in out Context;
                        First : in Natural;
                        Last  : in Natural);

   --  Get the first row index.
   function Get_First_Row_Index (From : in Context) return Natural;

   --  Get the last row index.
   function Get_Last_Row_Index (From : in Context) return Natural;

   --  Get the maximum number of rows that the SQL query can return.
   --  This operation uses the <b>sql-count</b> query.
   function Get_Max_Row_Count (From : in Context) return Natural;

   --  Get the SQL query that correspond to the query context.
   function Get_SQL (From   : in Context;
                     Driver : in ADO.Drivers.Driver_Index) return String;

   --  ------------------------------
   --  Query Definition
   --  ------------------------------
   --  The <b>Query_Definition</b> holds the SQL query pattern which is defined
   --  in an XML query file.  The query is identified by a name and a given XML
   --  query file can contain several queries.  The Dynamo generator generates
   --  one instance of <b>Query_Definition</b> for each query defined in the XML
   --  file.  The XML file is loaded during application initialization (or later)
   --  to get the SQL query pattern.
   type Query_Definition is limited record
      --  The query name.
      Name   : Util.Strings.Name_Access;

      --  The query file in which the query is defined.
      File   : Query_File_Access;

      --  The next query defined in the query file.
      Next   : Query_Definition_Access;

      --  The SQL query pattern (initialized when reading the XML query file).
      Query  : Query_Info_Access;
   end record;

   function Get_SQL (From   : in Query_Definition_Access;
                     Driver : in ADO.Drivers.Driver_Index) return String;

   --  ------------------------------
   --  Query File
   --  ------------------------------
   --  The <b>Query_File</b> describes the SQL queries associated and loaded from
   --  a given XML query file.  The Dynamo generator generates one instance of
   --  <b>Query_File</b> for each XML query file that it has read.  The Path,
   --  Sha1_Map, Queries and Next are initialized statically by the generator (during
   --  package elaboration).
   type Query_File is limited record
      --  Query relative path name
      Path          : Util.Strings.Name_Access;

      --  The SHA1 hash of the query map section.
      Sha1_Map      : Util.Strings.Name_Access;

      --  Stamp when the query file will be checked.
      Next_Check    : Interfaces.Unsigned_32;

      --  Stamp identifying the modification date of the query file.
      Last_Modified : Interfaces.Unsigned_32;

      --  The first query defined for that file.
      Queries       : Query_Definition_Access;

      --  The next XML query file registered in the application.
      Next          : Query_File_Access;
   end record;

private

   type Context is new ADO.SQL.Query with record
      First      : Natural := 0;
      Last       : Natural := 0;
      Last_Index : Natural := 0;
      Max_Row_Count : Natural := 0;
      Query_Def  : Query_Definition_Access := null;
   end record;

   --  Find the query with the given name.
   --  Returns the query definition that matches the name or null if there is none
   function Find_Query (File : in Query_File;
                        Name : in String) return Query_Definition_Access;

   use Ada.Strings.Unbounded;

   --  SQL query pattern
   type Query_Pattern is limited record
      SQL : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Query_Pattern_Array is array (ADO.Drivers.Driver_Index) of Query_Pattern;

   type Query_Info is new Util.Refs.Ref_Entity with record
      Main_Query  : Query_Pattern_Array;
      Count_Query : Query_Pattern_Array;
   end record;

--     package Query_Info_Ref is
--        new Util.Refs.References (Query_Info, Query_Info_Access);

end ADO.Queries;
