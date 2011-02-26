-----------------------------------------------------------------------
--  ADO Statements -- Database statements
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with Ada.Finalization;
with Ada.Calendar;

with ADO.Schemas;
with ADO.Parameters;
with ADO.SQL;
with ADO.Objects;
private with Ada.Unchecked_Conversion;
private with System;
private with Interfaces.C;

--
--  Represents SQL statements (prepared or not) and their result upon execution
--
--
--  Stmt : Query_Statement := Connection.Create_Statement
--              ("select * from user where name = :name");
--
--  Stmt.Bind_Param ("name", name);
--  Stmt.Execute;
--  while Stmt.Has_Elements loop
--     Id := Stmt.Get_Identifier (1);
--     ...
--  end loop;
--
package ADO.Statements is

   use Ada.Strings.Unbounded;

   Invalid_Statement : exception;

   Invalid_Column    : exception;

   Invalid_Type      : exception;

   --  ---------
   --  SQL statement
   --  ---------
   --  The SQL statement
   --
   type Statement is abstract new ADO.Parameters.Abstract_List with private;
   type Statement_Access is access all Statement'Class;

   function Get_Query (Query : Statement) return ADO.SQL.Query_Access;

   procedure Add_Parameter (Query : in out Statement;
                            Param : in ADO.Parameters.Parameter);

   procedure Set_Parameters (Query : in out Statement;
                             From  : in ADO.Parameters.Abstract_List'Class);

   --  Return the number of parameters in the list.
   function Length (Query : in Statement) return Natural;

   --  Return the parameter at the given position
   function Element (Query    : in Statement;
                     Position : in Natural) return ADO.Parameters.Parameter;

   --  Clear the list of parameters.
   procedure Clear (Query : in out Statement);

   subtype Query_String is String;

   procedure Add_Param (Params : in out Statement;
                        Value : in ADO.Objects.Object_Key);

   --  Operations to build the SQL query
   procedure Append (Query : in out Statement; SQL : in String);
   procedure Append (Query : in out Statement; SQL : in Unbounded_String);
   procedure Append (Query : in out Statement; Value : in Integer);
   procedure Append (Query : in out Statement; Value : in Long_Integer);

   --  Append the value to the SQL query string.
   procedure Append_Escape (Query : in out Statement; Value : in String);

   --  Append the value to the SQL query string.
   procedure Append_Escape (Query : in out Statement;
                            Value : in Unbounded_String);

   procedure Set_Filter (Query : in out Statement;
                         Filter : in String);

   --  Execute the query
   procedure Execute (Query : in out Statement) is abstract;

   --  ------------------------------
   --  An SQL query statement
   --
   --  The Query_Statement provides operations to retrieve the
   --  results after execution of the query.
   --  ------------------------------
   type Query_Statement is new Statement with private;
   type Query_Statement_Access is access all Query_Statement'Class;

   function Create_Statement (Proxy : Query_Statement_Access) return Query_Statement;

   --  Execute the query
   overriding
   procedure Execute (Query : in out Query_Statement);

   --  Get the number of rows returned by the query
   function Get_Row_Count (Query : in Query_Statement) return Natural;

   --  Returns True if there is more data (row) to fetch
   function Has_Elements (Query : in Query_Statement) return Boolean;

   --  Fetch the next row
   procedure Next (Query : in out Query_Statement);

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Int64 (Query  : Query_Statement;
                       Column : Natural) return Int64;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Identifier</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Identifier (Query  : Query_Statement;
                            Column : Natural) return Identifier;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Integer</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Integer (Query  : Query_Statement;
                         Column : Natural) return Integer;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Unbounded_String (Query  : Query_Statement;
                                  Column : Natural) return Unbounded_String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_String (Query : Query_Statement;
                        Column : Natural) return String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Time (Query  : Query_Statement;
                      Column : Natural) return Ada.Calendar.Time;

   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Column_Type (Query  : Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type;

--   function Get_Result_Integer (Query : Query_Statement) return Integer;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------
   type Delete_Statement is new Statement with private;
   type Delete_Statement_Access is access all Delete_Statement'Class;

   --  Execute the delete query.
   overriding
   procedure Execute (Query  : in out Delete_Statement);

   --  Execute the delete query.
   --  Returns the number of rows deleted.
   procedure Execute (Query  : in out Delete_Statement;
                      Result : out Natural);

   --  Create the delete statement
   function Create_Statement (Proxy : Delete_Statement_Access) return Delete_Statement;

   --  ------------------------------
   --  Update statement
   --  ------------------------------
   type Update_Statement is new Statement with private;
   type Update_Statement_Access is access all Update_Statement'Class;

   --  Get the update query object associated with this update statement.
   function Get_Update_Query (Update : in Update_Statement)
                              return ADO.SQL.Update_Query_Access;

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Boolean);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Integer);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Long_Integer);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Identifier);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in String);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Unbounded_String);

   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in ADO.Objects.Object_Key);

   --  Prepare the update/insert query to save the table field.
   --  identified by <b>Name</b> and set it to the identifier key held by <b>Value</b>.
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in ADO.Objects.Object_Ref'Class);

   --  Check if the update/insert query has some fields to update.
   function Has_Save_Fields (Update : in Update_Statement) return Boolean;

   --  Execute the query
   overriding
   procedure Execute (Query : in out Update_Statement);

   --  Create an update statement
   function Create_Statement (Proxy : Update_Statement_Access) return Update_Statement;

   --  Execute the query
   procedure Execute (Query : in out Update_Statement;
                      Result : out Integer);

   --  ------------------------------
   --  Insert statement
   --  ------------------------------
   type Insert_Statement is new Update_Statement with private;
   type Insert_Statement_Access is access all Insert_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Query : in out Insert_Statement);

   --  Create the insert statement.
   overriding
   function Create_Statement (Proxy : Update_Statement_Access) return Insert_Statement;

private

   type Statement is abstract new ADO.Parameters.Abstract_List with record
      Query  : ADO.SQL.Query_Access;
   end record;

   type Driver_Statement is new Ada.Finalization.Limited_Controlled with null record;

   procedure Execute (Query  : in out Statement;
                      SQL    : in Unbounded_String;
                      Params : in ADO.Parameters.Abstract_List'Class);

   type Query_Statement is new Statement with record
      Proxy       : Query_Statement_Access := null;
      Ref_Counter : Natural := 0;
   end record;

   overriding
   procedure Adjust (Stmt : in out Query_Statement);

   overriding
   procedure Finalize (Stmt : in out Query_Statement);

   --  String pointer to interface with a C library
   type Chars_Ptr is access all Character;
   pragma Convention (C, Chars_Ptr);

   type Size_T is mod 2 ** Standard'Address_Size;

   use Interfaces.C;

   function To_Chars_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Chars_Ptr);

   function To_Address is
     new Ada.Unchecked_Conversion (Chars_Ptr, System.Address);

   function "+" (Left : Chars_Ptr; Right : Size_T) return Chars_Ptr;
   pragma Inline ("+");

   --  Get an unsigned 64-bit number from a C string terminated by \0
   function Get_Uint64 (Str : Chars_Ptr) return unsigned_long;

   --  Get a signed 64-bit number from a C string terminated by \0
   function Get_Int64 (Str : Chars_Ptr) return Int64;

   type Delete_Statement is new Statement with record
      Proxy : Delete_Statement_Access := null;
      Ref_Counter : Natural := 0;
   end record;

   overriding
   procedure Adjust (Stmt : in out Delete_Statement);

   overriding
   procedure Finalize (Stmt : in out Delete_Statement);

   type Update_Statement is new Statement with record
      Proxy  : Update_Statement_Access := null;
      Update : ADO.SQL.Update_Query_Access;
      Ref_Counter : Natural := 0;
   end record;

   overriding
   procedure Adjust (Stmt : in out Update_Statement);

   overriding
   procedure Finalize (Stmt : in out Update_Statement);

   type Insert_Statement is new Update_Statement with record
      Pos2 : Natural := 0;
   end record;

end ADO.Statements;
