-----------------------------------------------------------------------
--  ADO Mysql Database -- MySQL Database connections
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
with System;
with Mysql.Mysql;  use Mysql.Mysql;

with ADO.SQL;
with ADO.Schemas;

package ADO.Statements.Mysql is

   --  ------------------------------
   --  Delete statement
   --  ------------------------------
   type Mysql_Delete_Statement is new Delete_Statement with private;
   type Mysql_Delete_Statement_Access is access all Mysql_Delete_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Mysql_Delete_Statement;
                      Result : out Natural);

   --  Create the delete statement
   overriding
   function Create_Statement (Proxy : Delete_Statement_Access) return Mysql_Delete_Statement;

   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access;

   --  ------------------------------
   --  Update statement
   --  ------------------------------
   type Mysql_Update_Statement is new Update_Statement with private;
   type Mysql_Update_Statement_Access is access all Mysql_Update_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Mysql_Update_Statement);

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Mysql_Update_Statement;
                      Result : out Integer);

   --  Create an update statement
   overriding
   function Create_Statement (Proxy : Update_Statement_Access) return Mysql_Update_Statement;

   --  Create an update statement
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------
   type Mysql_Insert_Statement is new Insert_Statement with private;
   type Mysql_Insert_Statement_Access is access all Mysql_Insert_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Mysql_Insert_Statement;
                      Result : out Integer);

   --  Create the insert statement.
   overriding
   function Create_Statement (Proxy : Update_Statement_Access) return Mysql_Insert_Statement;

   --  Create an insert statement.
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access;

   --  ------------------------------
   --  Query statement for MySQL
   --  ------------------------------
   type Mysql_Query_Statement is new Query_Statement with private;
   type Mysql_Query_Statement_Access is access all Mysql_Query_Statement'Class;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Mysql_Query_Statement);

   --  Get the number of rows returned by the query
   overriding
   function Get_Row_Count (Query : in Mysql_Query_Statement) return Natural;

   --  Returns True if there is more data (row) to fetch
   overriding
   function Has_Elements (Query : in Mysql_Query_Statement) return Boolean;

   --  Fetch the next row
   overriding
   procedure Next (Query : in out Mysql_Query_Statement);

   --  Returns true if the column <b>Column</b> is null.
   overriding
   function Is_Null (Query  : in Mysql_Query_Statement;
                     Column : in Natural) return Boolean;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Int64 (Query  : Mysql_Query_Statement;
                       Column : Natural) return Int64;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Unbounded_String (Query  : Mysql_Query_Statement;
                                  Column : Natural) return Unbounded_String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_String (Query  : Mysql_Query_Statement;
                        Column : Natural) return String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Time (Query  : Mysql_Query_Statement;
                      Column : Natural) return Ada.Calendar.Time;

   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Type (Query  : Mysql_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type;

   overriding
   procedure Finalize (Query : in out Mysql_Query_Statement);

   --  Create the query statement.
   overriding
   function Create_Statement (Proxy : Query_Statement_Access) return Mysql_Query_Statement;

   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access;

   function Create_Statement (Database : in Mysql_Access;
                              Query    : in String)
                              return Query_Statement_Access;

private
   type State is (HAS_ROW, HAS_MORE, DONE, ERROR);

   type Fields is array (Natural) of System.Address;
   type Row_Fields is access Fields;

   type Lengths is array (Natural) of Natural;
   type Lengths_Access is access Lengths;

   type Mysql_Query_Statement is new Query_Statement with record
      This_Query : aliased ADO.SQL.Query;
      Connection : Mysql_Access;
      Result     : MYSQL_RES;
      Row        : System_Access;
      Counter    : Natural := 1;
      Status     : State := DONE;
      Lengths    : Lengths_Access;
      Max_Column : Natural;
   end record;

   --  Get a column field address.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   function Get_Field (Query  : Mysql_Query_Statement'Class;
                       Column : Natural) return Chars_Ptr;

   type Mysql_Delete_Statement is new Delete_Statement with record
      Connection : Mysql_Access;
      Table      : ADO.Schemas.Class_Mapping_Access;
      Delete_Query : aliased ADO.SQL.Query;
   end record;

   type Mysql_Update_Statement is new Update_Statement with record
      Connection : Mysql_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

   type Mysql_Insert_Statement is new Insert_Statement with record
      Connection : Mysql_Access;
      This_Query : aliased ADO.SQL.Update_Query;
      Table      : ADO.Schemas.Class_Mapping_Access;
   end record;

end ADO.Statements.Mysql;
