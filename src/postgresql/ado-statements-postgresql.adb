-----------------------------------------------------------------------
--  ado-statements-postgresql -- Postgresql query statements
--  Copyright (C) 2018, 2019 Stephane Carrez
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

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

with Util.Log;
with Util.Log.Loggers;

with ADO.C;
with ADO.Sessions;
with ADO.Dialects;
with PQ.Perfect_Hash;
package body ADO.Statements.Postgresql is

   use type Interfaces.C.Strings.chars_ptr;
   use type PQ.PGresult_Access;
   use type PQ.ExecStatusType;
   use type ADO.Schemas.Class_Mapping_Access;
   use type ADO.Schemas.Column_Index;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Statements.Postgresql");

   type Dialect is new ADO.Dialects.Dialect with null record;

   --  Check if the string is a reserved keyword.
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean;

   --  Append the item in the buffer escaping some characters if necessary.
   --  The default implementation only escapes the single quote ' by doubling them.
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in String);

   --  Append the item in the buffer escaping some characters if necessary
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref);

   --  Append the boolean item in the buffer.
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in Boolean);

   --  Get the quote character to escape an identifier.
   overriding
   function Get_Identifier_Quote (D : in Dialect) return Character;

   Postgresql_Dialect : aliased Dialect;

   --  Execute the SQL query on the given Postgresql connection.
   --  Returns the query execution status
   function Execute (Connection : in PQ.PGconn_Access;
                     Query      : in String) return PQ.PGresult_Access;

   function Get_Affected_Rows (Result : in PQ.PGresult_Access) return Natural;

   --  ------------------------------
   --  Check if the string is a reserved keyword.
   --  ------------------------------
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean is
      pragma Unreferenced (D);
   begin
      return PQ.Perfect_Hash.Is_Keyword (Name);
   end Is_Reserved;

   --  ------------------------------
   --  Get the quote character to escape an identifier.
   --  ------------------------------
   overriding
   function Get_Identifier_Quote (D : in Dialect) return Character is
      pragma Unreferenced (D);
   begin
      return '"';
   end Get_Identifier_Quote;

   --  ------------------------------
   --  Append the item in the buffer escaping some characters if necessary.
   --  The default implementation only escapes the single quote ' by doubling them.
   --  ------------------------------
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in String) is
      pragma Unreferenced (D);

      C  : Character;
   begin
      Append (Buffer, 'E');
      Append (Buffer, ''');
      for I in Item'Range loop
         C := Item (I);
         case C is
            when ASCII.NUL =>
               Append (Buffer, '\');
               Append (Buffer, '0');

            when ASCII.CR =>
               Append (Buffer, '\');
               Append (Buffer, 'r');

            when ASCII.LF =>
               Append (Buffer, '\');
               Append (Buffer, 'n');

            when '\' | ''' | '"' =>
               Append (Buffer, '\');
               Append (Buffer, C);

            when others =>
               Append (Buffer, C);

         end case;
      end loop;
      Append (Buffer, ''');
   end Escape_Sql;

   --  ------------------------------
   --  Append the item in the buffer escaping some characters if necessary
   --  ------------------------------
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref) is
      pragma Unreferenced (D);

      use type Ada.Streams.Stream_Element;

      Hex  : constant String := "0123456789ABCDEF";
      C    : Ada.Streams.Stream_Element;
      Blob : constant ADO.Blob_Access := Item.Value;
   begin
      Append (Buffer, "BYTEA(E'\\x");
      for I in Blob.Data'Range loop
         C := Blob.Data (I);
         Append (Buffer, Hex (1 + Natural (C / 16)));
         Append (Buffer, Hex (1 + Natural (C mod 16)));
      end loop;
      Append (Buffer, "')");
   end Escape_Sql;

   --  ------------------------------
   --  Append the boolean item in the buffer.
   --  ------------------------------
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in Boolean) is
      pragma Unreferenced (D);
   begin
      Append (Buffer, (if Item then "TRUE" else "FALSE"));
   end Escape_Sql;

   function Get_Affected_Rows (Result : in PQ.PGresult_Access) return Natural is
      Count : PQ.chars_ptr;
   begin
      if PQ.PQresultStatus (Result) /= PQ.PGRES_COMMAND_OK then
         return 0;
      end if;

      Count := PQ.PQcmdTuples (Result);
      declare
         Val : constant String := Interfaces.C.Strings.Value (Count);
      begin
         if Val'Length = 0 then
            return 0;
         else
            return Natural'Value (Val);
         end if;

      exception
         when Constraint_Error =>
            Log.Info ("Affected rows is not a number: {0}", Val);
            return 0;
      end;
   end Get_Affected_Rows;

   --  ------------------------------
   --  Execute the SQL query on the given Postgresql connection.
   --  Returns the query execution status
   --  ------------------------------
   function Execute (Connection : in PQ.PGconn_Access;
                     Query      : in String) return PQ.PGresult_Access is
      use type PQ.PGconn_Access;

      Sql    : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Query);
      Result : PQ.PGresult_Access;
      Status : PQ.ExecStatusType;
   begin
      Log.Debug ("Execute {0}", Query);

      if Connection = PQ.Null_PGconn then
         Log.Warn ("Database connection is closed");
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;

      --  Execute the query
      Result := PQ.PQexec (Connection, ADO.C.To_C (Sql));
      Status := PQ.PQresultStatus (Result);
      case Status is
         when PQ.PGRES_COMMAND_OK | PQ.PGRES_TUPLES_OK =>
            null;

         when others =>
            declare
               Error : constant Strings.chars_ptr := PQ.PQresultErrorMessage (Result);
               Msg   : constant String := Strings.Value (Error);
            begin
               Log.Error ("Error: {0}", Msg);
               PQ.PQclear (Result);
               raise ADO.Statements.SQL_Error with "SQL error: " & Msg;
            end;

      end case;
      return Result;
   end Execute;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Postgresql_Delete_Statement;
                      Result : out Natural) is
   begin
      ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => "DELETE FROM ");
      ADO.SQL.Append_Name (Target => Stmt.Query.SQL, Name => Stmt.Table.Table.all);
      if Stmt.Query.Has_Join then
         ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => Stmt.Query.Get_Join);
      end if;
      if Stmt.Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => Stmt.Query.Get_Filter);
      end if;

      declare
         Sql_Query     : constant String := Stmt.Query.Expand;
         Delete_Result : PQ.PGresult_Access;
      begin
         Delete_Result := Execute (Stmt.Connection, Sql_Query);

         Result := Get_Affected_Rows (Delete_Result);
         PQ.PQclear (Delete_Result);
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Deleted {0} rows", Natural'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create the delete statement
   --  ------------------------------
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
      Result : constant Postgresql_Delete_Statement_Access := new Postgresql_Delete_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Query      := Result.Delete_Query'Access;
      Result.Delete_Query.Set_Dialect (Postgresql_Dialect'Access);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Update statement
   --  ------------------------------

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Postgresql_Update_Statement) is
      Result : Integer;
   begin
      Postgresql_Update_Statement'Class (Stmt).Execute (Result);
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Postgresql_Update_Statement;
                      Result : out Integer) is
   begin
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => "UPDATE ");
      ADO.SQL.Append_Name (Target => Stmt.This_Query.SQL, Name => Stmt.Table.Table.all);
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " SET ");
      ADO.SQL.Append_Fields (Update => Stmt.This_Query);
      if Stmt.This_Query.Has_Join then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Join);
      end if;
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
      end if;

      declare
         Sql_Query     : constant String := Stmt.This_Query.Expand;
         Update_Result : PQ.PGresult_Access;
      begin
         Update_Result := Execute (Stmt.Connection, Sql_Query);
         Result := Get_Affected_Rows (Update_Result);
         PQ.PQclear (Update_Result);
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Updated {0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an update statement.
   --  ------------------------------
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
      Result : constant Postgresql_Update_Statement_Access := new Postgresql_Update_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Update     := Result.This_Query'Access;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Postgresql_Dialect'Access);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Postgresql_Insert_Statement;
                      Result : out Integer) is
   begin
      if Stmt.Table /= null then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => "INSERT INTO ");
         ADO.SQL.Append_Name (Target => Stmt.This_Query.SQL, Name => Stmt.Table.Table.all);
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " (");
         ADO.SQL.Append_Fields (Update => Stmt.This_Query, Mode => False);
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => ") VALUES(");
         ADO.SQL.Append_Fields (Update => Stmt.This_Query, Mode => True);
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => ")");
      end if;
      declare
         Sql_Query     : constant String := Stmt.This_Query.Expand;
         Insert_Result : PQ.PGresult_Access;
      begin
         Insert_Result := Execute (Stmt.Connection, Sql_Query);
         Result := Get_Affected_Rows (Insert_Result);
         PQ.PQclear (Insert_Result);
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Inserted {0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
      Result : constant Postgresql_Insert_Statement_Access := new Postgresql_Insert_Statement;
   begin
      Result.Connection := Database;
      Result.Table  := Table;
      Result.Update := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Postgresql_Dialect'Access);
      ADO.SQL.Set_Insert_Mode (Result.This_Query);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Query statement for Postgresql
   --  ------------------------------

   --  ------------------------------
   --  Get a column field address.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field (Query  : in Postgresql_Query_Statement'Class;
                       Column : in Natural) return chars_ptr is
      Result : PQ.chars_ptr;
   begin
      if Query.Result = PQ.Null_PGresult then
         raise Invalid_Statement with "Null statement";
      end if;
      if Column >= Query.Max_Column then
         Log.Warn ("Column {0} is not valid", Natural'Image (Column));
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      if PQ.PQgetisnull (Query.Result, Query.Row, Interfaces.C.int (Column)) = 1 then
         Log.Warn ("Null column {0} in row {1}", Natural'Image (Column),
                   Interfaces.C.int'Image (Query.Row));
         raise Invalid_Type with "Null column";
      end if;
      Result := PQ.PQgetvalue (Query.Result, Query.Row, Interfaces.C.int (Column));
      return ADO.Statements.To_Chars_Ptr (Result);
   end Get_Field;

   --  ------------------------------
   --  Get a column field length.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field_Length (Query  : in Postgresql_Query_Statement'Class;
                              Column : in Natural) return Natural is
   begin
      if Query.Row >= Query.Row_Count then
         raise Invalid_Statement with "Null statement";
      end if;
      if Column >= Query.Max_Column then
         Log.Warn ("Column {0} is not valid", Natural'Image (Column));
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      return Natural (PQ.PQgetlength (Query.Result, Query.Row, Interfaces.C.int (Column)));
   end Get_Field_Length;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt : in out Postgresql_Query_Statement) is
   begin
      if Stmt.This_Query.Has_Join then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Join);
      end if;
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
      end if;

      if Stmt.Result /= PQ.Null_PGresult then
         PQ.PQclear (Stmt.Result);
         Stmt.Result := PQ.Null_PGresult;
      end if;
      declare
         Expanded_Query : constant String := Stmt.Query.Expand;
         Status         : PQ.ExecStatusType;
      begin
         --  Execute the query
         Stmt.Result := Execute (Stmt.Connection, Expanded_Query);

         --  Report an error if the query failed
         Status := PQ.PQresultStatus (Stmt.Result);
         if Status /= PQ.PGRES_TUPLES_OK and Status /= PQ.PGRES_COMMAND_OK then
            declare
               Message : constant String := Strings.Value (PQ.PQerrorMessage (Stmt.Connection));
            begin
               Log.Error ("Query failed: '{0}'", Expanded_Query);
               Log.Error ("  with error: '{0}'", Message);
               raise ADO.Statements.SQL_Error with "Query failed: " & Message;
            end;
         end if;
         Stmt.Row := 0;
         Stmt.Row_Count := PQ.PQntuples (Stmt.Result);
         Stmt.Max_Column := Natural (PQ.PQnfields (Stmt.Result));
      end;
   end Execute;

   --  ------------------------------
   --  Get the number of rows returned by the query
   --  ------------------------------
   overriding
   function Get_Row_Count (Query : in Postgresql_Query_Statement) return Natural is
   begin
      return Natural (Query.Row_Count);
   end Get_Row_Count;

   --  ------------------------------
   --  Returns True if there is more data (row) to fetch
   --  ------------------------------
   overriding
   function Has_Elements (Query : in Postgresql_Query_Statement) return Boolean is
   begin
      return Query.Row < Query.Row_Count;
   end Has_Elements;

   --  ------------------------------
   --  Fetch the next row
   --  ------------------------------
   overriding
   procedure Next (Query : in out Postgresql_Query_Statement) is
   begin
      if Query.Row < Query.Row_Count then
         Query.Row := Query.Row + 1;
      else
         Log.Warn ("Next has no more element in the result set.");
      end if;
   end Next;

   --  ------------------------------
   --  Returns true if the column <b>Column</b> is null.
   --  ------------------------------
   overriding
   function Is_Null (Query  : in Postgresql_Query_Statement;
                     Column : in Natural) return Boolean is
   begin
      if Query.Result = PQ.Null_PGresult then
         return True;
      end if;
      if Column >= Query.Max_Column or Query.Row >= Query.Row_Count then
         return True;
      end if;
      return PQ.PQgetisnull (Query.Result, Query.Row, Interfaces.C.int (Column)) = 1;
   end Is_Null;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Int64 (Query  : Postgresql_Query_Statement;
                       Column : Natural) return Int64 is
      Field  : constant chars_ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         raise Invalid_Type with "NULL cannot be converted to Integer";
      else
         return Get_Int64 (Field);
      end if;
   end Get_Int64;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Long_Float</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Double (Query  : Postgresql_Query_Statement;
                        Column : Natural) return Long_Float is
      Field  : constant chars_ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         raise Invalid_Type with "NULL cannot be converted to Long_Float";
      else
         return Get_Double (Field);
      end if;
   end Get_Double;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Boolean</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Boolean (Query  : Postgresql_Query_Statement;
                         Column : Natural) return Boolean is
      Field  : chars_ptr := Query.Get_Field (Column);
      C      : Character;
   begin
      if Field = null then
         raise Invalid_Type with "NULL cannot be converted to Boolean";
      end if;
      C := Field.all;
      Field := Field + 1;
      case C is
         when '0' =>
            if Field.all /= ASCII.NUL then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            return False;

         when '1' =>
            if Field.all /= ASCII.NUL then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            return True;

         when 'T' | 't' =>
            C := Field.all;
            if C = ASCII.NUL then
               return True;
            end if;
            if C /= 'r' and C /= 'R' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            C := Field.all;
            if C /= 'u' and C /= 'U' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            C := Field.all;
            if C /= 'e' and C /= 'E' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            return True;

         when 'F' | 'f' =>
            C := Field.all;
            if C = ASCII.NUL then
               return False;
            end if;
            if C /= 'a' and C /= 'A' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            C := Field.all;
            if C /= 'l' and C /= 'L' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            C := Field.all;
            if C /= 's' and C /= 'S' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            C := Field.all;
            if C /= 'e' and C /= 'E' then
               raise Invalid_Type with "Invalid boolean value";
            end if;
            return False;

         when others =>
            raise Invalid_Type with "Invalid boolean value";

      end case;
   end Get_Boolean;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Unbounded_String (Query  : Postgresql_Query_Statement;
                                  Column : Natural) return Unbounded_String is
      Field  : chars_ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         raise Invalid_Type with "NULL cannot be converted to String";
      end if;
      declare
         Result : Unbounded_String;
         C      : Character;
      begin
         loop
            C := Field.all;
            exit when C = ASCII.NUL;
            Append (Result, C);
            Field := Field + 1;
         end loop;
         return Result;
      end;
   end Get_Unbounded_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_String (Query  : Postgresql_Query_Statement;
                        Column : Natural) return String is
   begin
      return To_String (Query.Get_Unbounded_String (Column));
   end Get_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   --  ------------------------------
   overriding
   function Get_Blob (Query  : in Postgresql_Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref is
      Field   : constant chars_ptr := Query.Get_Field (Column);
   begin
      if Field /= null then
         return Get_Blob (Size => Query.Get_Field_Length (Column),
                          Data => Field);
      else
         return Null_Blob;
      end if;
   end Get_Blob;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Time (Query  : Postgresql_Query_Statement;
                      Column : Natural) return Ada.Calendar.Time is
      Field  : constant chars_ptr    := Query.Get_Field (Column);
   begin
      return ADO.Statements.Get_Time (Field);
   end Get_Time;

   --  ------------------------------
   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Column_Type (Query  : Postgresql_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type is
   begin
      if Query.Result = PQ.Null_PGresult then
         raise Invalid_Statement with "No statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      end if;
      return ADO.Schemas.T_UNKNOWN;
   end Get_Column_Type;

   --  ------------------------------
   --  Get the column name
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Column_Name (Query  : in Postgresql_Query_Statement;
                             Column : in Natural)
                             return String is

      Name : PQ.chars_ptr;
   begin
      if Query.Result = PQ.Null_PGresult then
         raise Invalid_Statement with "No statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      end if;

      Name := PQ.PQfname (Query.Result, Interfaces.C.int (Column));
      if Name = Interfaces.C.Strings.Null_Ptr then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      else
         return Interfaces.C.Strings.Value (Name);
      end if;
   end Get_Column_Name;

   --  ------------------------------
   --  Get the number of columns in the result.
   --  ------------------------------
   overriding
   function Get_Column_Count (Query  : in Postgresql_Query_Statement)
                              return Natural is
   begin
      if Query.Result = PQ.Null_PGresult then
         raise Invalid_Statement with "No statement";
      end if;
      return Query.Max_Column;
   end Get_Column_Count;

   overriding
   procedure Finalize (Query : in out Postgresql_Query_Statement) is
   begin
      if Query.Result /= PQ.Null_PGresult then
         PQ.PQclear (Query.Result);
         Query.Result := PQ.Null_PGresult;
      end if;
   end Finalize;

   function Create_Statement (Database : in PQ.PGconn_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
      Result : constant Postgresql_Query_Statement_Access := new Postgresql_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Postgresql_Dialect'Access);
      if Table /= null then
         ADO.SQL.Append (Target => Result.This_Query.SQL, SQL => "SELECT ");
         for I in Table.Members'Range loop
            if I > Table.Members'First then
               ADO.SQL.Append (Target => Result.This_Query.SQL, SQL => ", ");
            end if;
            ADO.SQL.Append (Target => Result.This_Query.SQL, SQL => "o.");
            ADO.SQL.Append_Name (Target => Result.This_Query.SQL, Name => Table.Members (I).all);
         end loop;
         ADO.SQL.Append (Target => Result.This_Query.SQL, SQL => " FROM ");
         ADO.SQL.Append_Name (Target => Result.This_Query.SQL, Name => Table.Table.all);
         ADO.SQL.Append (Target => Result.This_Query.SQL, SQL => " AS o ");
      end if;
      return Result.all'Access;
   end Create_Statement;

   function Create_Statement (Database : in PQ.PGconn_Access;
                              Query    : in String)
                              return Query_Statement_Access is
      Result : constant Postgresql_Query_Statement_Access := new Postgresql_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Postgresql_Dialect'Access);
      Append (Query => Result.all, SQL => Query);
      return Result.all'Access;
   end Create_Statement;

end ADO.Statements.Postgresql;
