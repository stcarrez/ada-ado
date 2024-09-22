-----------------------------------------------------------------------
--  ADO SQL -- Basic SQL Generation
--  Copyright (C) 2010, 2011, 2012, 2015, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Calendar;

with ADO.Parameters;
with ADO.Dialects;

--  Utilities for creating SQL queries and statements.
package ADO.SQL is

   use Ada.Strings.Unbounded;

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

   --  Clear the query object.
   overriding
   procedure Clear (Target : in out Query);

   --  Set the SQL dialect description object.
   overriding
   procedure Set_Dialect (Target : in out Query;
                          D      : in ADO.Dialects.Dialect_Access);

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
   overriding
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

   --  Set the SQL dialect description object.
   overriding
   procedure Set_Dialect (Target : in out Update_Query;
                          D      : in ADO.Dialects.Dialect_Access);

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
                         Value  : in Long_Float);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Identifier);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Entity_Type);

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
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in ADO.Blob_Ref);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to NULL.
   procedure Save_Null_Field (Update : in out Update_Query;
                              Name   : in String);

   --  Check if the update/insert query has some fields to update.
   function Has_Save_Fields (Update : in Update_Query) return Boolean;

   procedure Set_Insert_Mode (Update : in out Update_Query);

   procedure Append_Fields (Update : in out Update_Query;
                            Mode   : in Boolean := False);

   --  Read the file for SQL statements separated by ';' and execute the
   --  `Process` procedure with each SQL statement that is read.
   procedure Read_File (Path    : in String;
                        Process : not null access procedure (SQL : in String));

private

   type Buffer is new ADO.Parameters.List with record
      Buf     : Unbounded_String;
   end record;

   type Update_Query is new Query with record
      Pos            : Natural := 0;
      Set_Fields     : Buffer;
      Fields         : Buffer;
      Is_Update_Stmt : Boolean := True;
   end record;

   procedure Add_Field (Update : in out Update_Query'Class;
                        Name   : in String);

end ADO.SQL;
