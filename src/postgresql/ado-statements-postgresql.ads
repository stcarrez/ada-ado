-----------------------------------------------------------------------
--  ADO Postgresql statements -- Postgresql query statements
--  Copyright (C) 2018 Stephane Carrez
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

with ADO.SQL;
with ADO.Schemas;
with PQ;

package ADO.Statements.Postgresql is

   --  ------------------------------
   --  Delete statement
   --  ------------------------------
   type Postgresql_Delete_Statement is new Delete_Statement with private;
   type Postgresql_Delete_Statement_Access is access all Postgresql_Delete_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Postgresql_Delete_Statement;
                      Result : out Natural);

   --  Create the delete statement
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access;

   --  ------------------------------
   --  Update statement
   --  ------------------------------
   type Postgresql_Update_Statement is new Update_Statement with private;
   type Postgresql_Update_Statement_Access is access all Postgresql_Update_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Postgresql_Update_Statement);

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Postgresql_Update_Statement;
                      Result : out Integer);

   --  Create an update statement.
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------
   type Postgresql_Insert_Statement is new Insert_Statement with private;
   type Postgresql_Insert_Statement_Access is access all Postgresql_Insert_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Postgresql_Insert_Statement;
                      Result : out Integer);

   --  Create an insert statement.
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access;

   --  ------------------------------
   --  Query statement for Postgresql
   --  ------------------------------
   type Postgresql_Query_Statement is new Query_Statement with private;
   type Postgresql_Query_Statement_Access is access all Postgresql_Query_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Postgresql_Query_Statement);

   --  Get the number of rows returned by the query
   overriding
   function Get_Row_Count (Query : in Postgresql_Query_Statement) return Natural;

   --  Returns True if there is more data (row) to fetch
   overriding
   function Has_Elements (Query : in Postgresql_Query_Statement) return Boolean;

   --  Fetch the next row
   overriding
   procedure Next (Query : in out Postgresql_Query_Statement);

   --  Returns true if the column <b>Column</b> is null.
   overriding
   function Is_Null (Query  : in Postgresql_Query_Statement;
                     Column : in Natural) return Boolean;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Int64 (Query  : Postgresql_Query_Statement;
                       Column : Natural) return Int64;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Boolean</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Boolean (Query  : Postgresql_Query_Statement;
                         Column : Natural) return Boolean;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Unbounded_String (Query  : Postgresql_Query_Statement;
                                  Column : Natural) return Unbounded_String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_String (Query  : Postgresql_Query_Statement;
                        Column : Natural) return String;

   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   overriding
   function Get_Blob (Query  : in Postgresql_Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Time (Query  : Postgresql_Query_Statement;
                      Column : Natural) return Ada.Calendar.Time;

   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Type (Query  : Postgresql_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type;

   --  Get the column name.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Name (Query  : in Postgresql_Query_Statement;
                             Column : in Natural)
                             return String;

   --  Get the number of columns in the result.
   overriding
   function Get_Column_Count (Query  : in Postgresql_Query_Statement)
                              return Natural;

   overriding
   procedure Finalize (Query : in out Postgresql_Query_Statement);

   --  Create the query statement.
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access;

   function Create_Statement (Database : in PQ.PGconn_Access;
                              Query    : in String)
                              return Query_Statement_Access;

private
   type State is (HAS_ROW, HAS_MORE, DONE, ERROR);

   type Postgresql_Query_Statement is new Query_Statement with record
      This_Query : aliased ADO.SQL.Query;
      Connection : PQ.PGconn_Access;
      Result     : PQ.PGresult_Access;
      Row        : Interfaces.C.int := 0;
      Row_Count  : Interfaces.C.int := 0;
      Counter    : Natural := 1;
      Status     : State := DONE;
      Max_Column : Natural;
   end record;

   --  Get a column field address.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   function Get_Field (Query  : Postgresql_Query_Statement'Class;
                       Column : Natural) return chars_ptr;

   --  Get a column field length.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field_Length (Query  : in Postgresql_Query_Statement'Class;
                              Column : in Natural) return Natural;

   type Postgresql_Delete_Statement is new Delete_Statement with record
      Connection   : PQ.PGconn_Access;
      Table        : ADO.Schemas.Class_Mapping_Access;
      Delete_Query : aliased ADO.SQL.Query;
   end record;

   type Postgresql_Update_Statement is new Update_Statement with record
      Connection : PQ.PGconn_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

   type Postgresql_Insert_Statement is new Insert_Statement with record
      Connection : PQ.PGconn_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

end ADO.Statements.Postgresql;
