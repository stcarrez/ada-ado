-----------------------------------------------------------------------
--  ADO SQL -- Basic SQL Generation
--  Copyright (C) 2010 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Calendar;

with Util.Strings;
with ADO.Parameters;

--  Utilities for creating SQL queries and statements.
package ADO.SQL is

   use Ada.Strings.Unbounded;

   function Escape (Str : in Unbounded_String) return String;
   function Escape (Str : in String) return String;

   type Keyword_Array is array (Natural range <>) of Util.Strings.Name_Access;

   --  --------------------
   --  SQL Dialect
   --  --------------------
   --  The <b>Dialect</b> defines the specific characteristics that must be
   --  taken into account when building the SQL statement.  This includes:
   --  <ul>
   --    <li>The definition of reserved keywords that must be escaped</li>
   --    <li>How to escape those keywords</li>
   --  </ul>
   type Dialect is tagged private;
   type Dialect_Access is access all Dialect'Class;

   --  Check if the string is a reserved keyword.
   function Is_Reserved (D    : Dialect;
                         Name : String) return Boolean;

   --  Add a set of keywords to be escaped.
   procedure Add_Keywords (D        : in out Dialect;
                           Keywords : in Keyword_Array);

   --  Get the quote character to escape an identifier.
   function Get_Identifier_Quote (D : in Dialect) return Character;

   --  --------------------
   --  Buffer
   --  --------------------
   --  The <b>Buffer</b> type allows to build easily an SQL statement.
   --
   type Buffer is private;
   type Buffer_Access is access all Buffer;

   --  Clear the SQL buffer.
   procedure Clear (Target : in out Buffer);

   --  Append an SQL extract into the buffer.
   procedure Append (Target : in out Buffer;
                     SQL    : in String);

   --  Append a name in the buffer and escape that
   --  name if this is a reserved keyword.
   procedure Append_Name (Target : in out Buffer;
                          Name   : in String);

   --  Append a string value in the buffer and
   --  escape any special character if necessary.
   procedure Append_Value (Target : in out Buffer;
                           Value  : in String);

   --  Append a string value in the buffer and
   --  escape any special character if necessary.
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Unbounded_String);

   --  Append the integer value in the buffer.
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Long_Integer);

   --  Append the integer value in the buffer.
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Integer);

   --  Append the identifier value in the buffer.
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Identifier);

   --  Get the SQL string that was accumulated in the buffer.
   function To_String (From : in Buffer) return String;

   --  Get the SQL dialect description object.
   function Get_Dialect (From : in Buffer) return Dialect_Access;

   --  Get the SQL dialect description object.
   procedure Set_Dialect (Target : in out Buffer;
                          D      : in Dialect_Access);

   --  --------------------
   --  Query
   --  --------------------
   --  The <b>Query</b> type contains the elements for building and
   --  executing the request.  This includes:
   --  <ul>
   --    <li>The SQL request (full request or a fragment)</li>
   --    <li>The SQL request parameters </li>
   --    <li>An SQL filter condition</li>
   --  </ul>
   --
   type Query is new ADO.Parameters.List with record
      SQL    : Buffer;
      Filter : Buffer;
      Join   : Buffer;
   end record;
   type Query_Access is access all Query'Class;

   procedure Set_Filter (Target : in out Query;
                         Filter : in String);

   function Has_Filter (Source : in Query) return Boolean;

   function Get_Filter (Source : in Query) return String;

   --  Set the join condition.
   procedure Set_Join (Target : in out Query;
                       Join   : in String);

   --  Returns true if there is a join condition
   function Has_Join (Source : in Query) return Boolean;

   --  Get the join condition
   function Get_Join (Source : in Query) return String;

   --  Set the parameters from another parameter list.
   --  If the parameter list is a query object, also copy the filter part.
   procedure Set_Parameters (Params : in out Query;
                             From   : in ADO.Parameters.Abstract_List'Class);

   --  Expand the parameters into the query and return the expanded SQL query.
   function Expand (Source : in Query) return String;

   --  --------------------
   --  Update Query
   --  --------------------
   --  The <b>Update_Query</b> provides additional helper functions to construct
   --  and <i>insert</i> or an <i>update</i> statement.
   type Update_Query is new Query with private;
   type Update_Query_Access is access all Update_Query'Class;

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Boolean);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Integer);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Long_Long_Integer);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Identifier);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in String);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Unbounded_String);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to NULL.
   procedure Save_Null_Field (Update : in out Update_Query;
                              Name   : in String);

   --  Check if the update/insert query has some fields to update.
   function Has_Save_Fields (Update : in Update_Query) return Boolean;

   procedure Set_Insert_Mode (Update : in out Update_Query);

   procedure Append_Fields (Update : in out Update_Query;
                            Mode   : in Boolean := False);

private

   type Buffer is new ADO.Parameters.List with record
      Buf     : Unbounded_String;
      Dialect : Dialect_Access;
   end record;

   type Update_Query is new Query with record
      Pos            : Natural := 0;
      Set_Fields     : Buffer;
      Fields         : Buffer;
      Is_Update_Stmt : Boolean := True;
   end record;

   type Dialect is tagged record
      Keywords : Util.Strings.String_Set.Set;
   end record;

end ADO.SQL;
