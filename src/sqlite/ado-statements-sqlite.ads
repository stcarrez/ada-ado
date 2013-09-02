-----------------------------------------------------------------------
--  ADO Mysql Database -- MySQL Database connections
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

with System;
with ADO.Drivers.Connections.Sqlite;
package ADO.Statements.Sqlite is

   type Handle is null record;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------
   type Sqlite_Delete_Statement is new Delete_Statement with private;
   type Sqlite_Delete_Statement_Access is access all Sqlite_Delete_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Sqlite_Delete_Statement;
                      Result : out Natural);

   --  Create the delete statement
   function Create_Statement (Database : in ADO.Drivers.Connections.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access;

   --  ------------------------------
   --  Update statement
   --  ------------------------------
   type Sqlite_Update_Statement is new Update_Statement with private;
   type Sqlite_Update_Statement_Access is access all Sqlite_Update_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Sqlite_Update_Statement);

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Sqlite_Update_Statement;
                      Result : out Integer);

   --  Create an update statement
   function Create_Statement (Database : in ADO.Drivers.Connections.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------
   type Sqlite_Insert_Statement is new Insert_Statement with private;
   type Sqlite_Insert_Statement_Access is access all Sqlite_Insert_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Sqlite_Insert_Statement;
                      Result : out Integer);

   --  Create an insert statement.
   function Create_Statement (Database : in ADO.Drivers.Connections.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access;

   --  ------------------------------
   --  Query statement for SQLite
   --  ------------------------------
   type Sqlite_Query_Statement is new Query_Statement with private;
   type Sqlite_Query_Statement_Access is access all Sqlite_Query_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Query : in out Sqlite_Query_Statement);

   --  Get the number of rows returned by the query
   overriding
   function Get_Row_Count (Query : in Sqlite_Query_Statement) return Natural;

   --  Returns True if there is more data (row) to fetch
   overriding
   function Has_Elements (Query : in Sqlite_Query_Statement) return Boolean;

   --  Fetch the next row
   overriding
   procedure Next (Query : in out Sqlite_Query_Statement);

   --  Returns true if the column <b>Column</b> is null.
   overriding
   function Is_Null (Query  : in Sqlite_Query_Statement;
                     Column : in Natural) return Boolean;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Int64 (Query  : Sqlite_Query_Statement;
                       Column : Natural) return Int64;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Unbounded_String (Query  : Sqlite_Query_Statement;
                                  Column : Natural) return Unbounded_String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_String (Query  : Sqlite_Query_Statement;
                        Column : Natural) return String;

   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   overriding
   function Get_Blob (Query  : in Sqlite_Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Time (Query  : Sqlite_Query_Statement;
                      Column : Natural) return Ada.Calendar.Time;

   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Type (Query  : Sqlite_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type;

   --  Get the column name.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Name (Query  : in Sqlite_Query_Statement;
                             Column : in Natural)
                             return String;

   --  Get the number of columns in the result.
   overriding
   function Get_Column_Count (Query  : in Sqlite_Query_Statement)
                              return Natural;

   --  Deletes the query statement.
   overriding
   procedure Finalize (Query : in out Sqlite_Query_Statement);

   --  Create the query statement.
   function Create_Statement (Database : in ADO.Drivers.Connections.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access;

   --  Create the query statement.
   function Create_Statement (Database : in ADO.Drivers.Connections.Sqlite.Sqlite_Access;
                              Query    : in String)
                              return Query_Statement_Access;

private
   type State is (HAS_ROW, HAS_MORE, DONE, ERROR);

   type Sqlite_Query_Statement is new Query_Statement with record
      This_Query : aliased ADO.SQL.Query;
      Connection : ADO.Drivers.Connections.Sqlite.Sqlite_Access;
      Stmt       : ADO.Drivers.Connections.Sqlite.Sqlite_Access := System.Null_Address;
      Counter    : Natural := 1;
      Status     : State := DONE;
      Max_Column : Natural;
   end record;

   type Sqlite_Delete_Statement is new Delete_Statement with record
      Connection   : ADO.Drivers.Connections.Sqlite.Sqlite_Access;
      Table        : ADO.Schemas.Class_Mapping_Access;
      Delete_Query : aliased ADO.SQL.Query;
   end record;

   type Sqlite_Update_Statement is new Update_Statement with record
      Connection : ADO.Drivers.Connections.Sqlite.Sqlite_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

   type Sqlite_Insert_Statement is new Insert_Statement with record
      Connection : ADO.Drivers.Connections.Sqlite.Sqlite_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

end ADO.Statements.Sqlite;
