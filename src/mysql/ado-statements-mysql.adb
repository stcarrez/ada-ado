-----------------------------------------------------------------------
--  ado-statements-mysql -- MySQL Statements
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2018, 2019, 2021, 2022 Stephane Carrez
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

with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

with Util.Log;
with Util.Log.Loggers;

with System.Storage_Elements;
with Interfaces.C;
with Mysql.Com;  use Mysql.Com;
with Mysql.Perfect_Hash; use Mysql;
with ADO.C;
with ADO.Sessions;
with ADO.Dialects;
package body ADO.Statements.Mysql is

   use Util.Log;
   use System.Storage_Elements;
   use type ADO.Schemas.Class_Mapping_Access;
   use type ADO.Schemas.Column_Index;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Statements.Mysql");
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

   --  Get the quote character to escape an identifier.
   overriding
   function Get_Identifier_Quote (D : in Dialect) return Character;

   Mysql_Dialect : aliased Dialect;

   --  Execute the SQL query on the given mysql connection.
   --  Returns the query execution status
   function Execute (Connection : in Mysql_Access;
                     Query      : in String) return int;

   --  Check for an error after executing a mysql statement.
   procedure Check_Error (Connection : in Mysql_Access;
                          Result     : in int);
   pragma Inline (Check_Error);

   --  ------------------------------
   --  Check if the string is a reserved keyword.
   --  ------------------------------
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean is
      pragma Unreferenced (D);
   begin
      return Perfect_Hash.Is_Keyword (Name);
   end Is_Reserved;

   --  ------------------------------
   --  Get the quote character to escape an identifier.
   --  ------------------------------
   overriding
   function Get_Identifier_Quote (D : in Dialect) return Character is
      pragma Unreferenced (D);
   begin
      return '`';
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

      C    : Ada.Streams.Stream_Element;
      Blob : constant ADO.Blob_Accessor := Item.Value;
   begin
      Append (Buffer, ''');
      for I in Blob.Data'Range loop
         C := Blob.Data (I);
         case C is
            when Character'Pos (ASCII.NUL) =>
               Append (Buffer, '\');
               Append (Buffer, '0');

            when Character'Pos (ASCII.CR) =>
               Append (Buffer, '\');
               Append (Buffer, 'r');

            when Character'Pos (ASCII.LF) =>
               Append (Buffer, '\');
               Append (Buffer, 'n');

            when Character'Pos ('\') | Character'Pos (''') | Character'Pos ('"') =>
               Append (Buffer, '\');
               Append (Buffer, Character'Val (C));

            when others =>
               Append (Buffer, Character'Val (C));
         end case;
      end loop;
      Append (Buffer, ''');
   end Escape_Sql;

   --  ------------------------------
   --  Check for an error after executing a mysql statement.
   --  ------------------------------
   procedure Check_Error (Connection : in Mysql_Access;
                          Result     : in int) is
   begin
      if Result /= 0 then
         declare
            Error : constant Strings.chars_ptr := Mysql_Error (Connection);
            Msg   : constant String := Strings.Value (Error);
         begin
            Log.Error ("Error: {0}", Msg);
            raise ADO.Statements.SQL_Error with "SQL error: " & Msg;
         end;
      end if;
   end Check_Error;

   --  ------------------------------
   --  Execute the SQL query on the given mysql connection.
   --  Returns the query execution status
   --  ------------------------------
   function Execute (Connection : in Mysql_Access;
                     Query      : in String) return int is
      Sql : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Query);
   begin
      if Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      Log.Debug ("Execute {0}", Query);

      --  Execute the query
      return Mysql_Real_Query (Connection, ADO.C.To_C (Sql), Query'Length);
   end Execute;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Mysql_Delete_Statement;
                      Result : out Natural) is
   begin
      if Stmt.Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
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
         Sql_Query : constant String := Stmt.Query.Expand;
         Res : int;
      begin
         Res := Execute (Stmt.Connection, Sql_Query);
         Check_Error (Stmt.Connection, Res);

         Result := Natural (Mysql_Affected_Rows (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Deleted {0} rows", Natural'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create the delete statement
   --  ------------------------------
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
      Result : constant Mysql_Delete_Statement_Access := new Mysql_Delete_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Query      := Result.Delete_Query'Access;
      Result.Delete_Query.Set_Dialect (Mysql_Dialect'Access);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Update statement
   --  ------------------------------

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Mysql_Update_Statement) is
      Result : Integer;
   begin
      Mysql_Update_Statement'Class (Stmt).Execute (Result);
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Mysql_Update_Statement;
                      Result : out Integer) is
   begin
      if Stmt.Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => "UPDATE ");
      ADO.SQL.Append_Name (Target => Stmt.This_Query.SQL, Name => Stmt.Table.Table.all);
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " AS o SET ");
      ADO.SQL.Append_Fields (Update => Stmt.This_Query);
      if Stmt.This_Query.Has_Join then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Join);
      end if;
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
      end if;

      declare
         Sql_Query : constant String := Stmt.This_Query.Expand;
         Res       : int;
      begin
         Res := Execute (Stmt.Connection, Sql_Query);
         Check_Error (Stmt.Connection, Res);
         Result := Integer (Mysql_Affected_Rows (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Updated {0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an update statement.
   --  ------------------------------
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
      Result : constant Mysql_Update_Statement_Access := new Mysql_Update_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Update     := Result.This_Query'Access;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Mysql_Dialect'Access);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Mysql_Insert_Statement;
                      Result : out Integer) is
   begin
      if Stmt.Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
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
         Sql_Query : constant String := Stmt.This_Query.Expand;
         Res       : int;
      begin
         Res := Execute (Stmt.Connection, Sql_Query);
         Check_Error (Stmt.Connection, Res);
         Result := Integer (Mysql_Affected_Rows (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Inserted {0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
      Result : constant Mysql_Insert_Statement_Access := new Mysql_Insert_Statement;
   begin
      Result.Connection := Database;
      Result.Table  := Table;
      Result.Update := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Mysql_Dialect'Access);
      ADO.SQL.Set_Insert_Mode (Result.This_Query);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Query statement for MySQL
   --  ------------------------------

   type Unsigned_Long_Ptr is access all unsigned_long;

   function To_System_Access is
     new Ada.Unchecked_Conversion (System.Address, System_Access);

   function To_Address is
     new Ada.Unchecked_Conversion (System_Access, System.Address);

   function To_Unsigned_Long_Ptr is
     new Ada.Unchecked_Conversion (System_Access, Unsigned_Long_Ptr);

   function "+" (Left : System_Access; Right : Size_T) return System_Access;
   pragma Inline_Always ("+");

   function "+" (Left : System_Access; Right : Size_T) return System_Access is
   begin
      return To_System_Access (To_Address (Left) + Storage_Offset (Right));
   end "+";

   --  ------------------------------
   --  Get a column field address.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field (Query  : in Mysql_Query_Statement'Class;
                       Column : in Natural) return chars_ptr is
      use System;

      R : System_Access;
   begin
      if Query.Row = null then
         raise Invalid_Statement with "Null statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      R := Query.Row + Size_T (Column * (R'Size / 8));
      return To_Chars_Ptr (R.all);
   end Get_Field;

   --  ------------------------------
   --  Get a column field length.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field_Length (Query  : in Mysql_Query_Statement'Class;
                              Column : in Natural) return Natural is
      R : System_Access;
   begin
      if Query.Row = null then
         raise Invalid_Statement with "Null statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      R := mysql_fetch_lengths (Query.Result);
      R := R + Size_T (Column * (R'Size / 8));
      return Natural (To_Unsigned_Long_Ptr (R).all);
   end Get_Field_Length;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt : in out Mysql_Query_Statement) is
      Result : int;
   begin
      if Stmt.Connection = null then
         Log.Warn ("Database connection is closed");
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      if Stmt.This_Query.Has_Join then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Join);
      end if;
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
      end if;

      if Stmt.Result /= null then
         Mysql_Free_Result (Stmt.Result);
         Stmt.Result := null;
      end if;
      declare
         Expanded_Query : constant String := Stmt.Query.Expand;
      begin
         --  Execute the query
         Result := Execute (Stmt.Connection, Expanded_Query);

         --  Report an error if the query failed
         if Result /= 0 then
            declare
               Message : constant String := Strings.Value (Mysql_Error (Stmt.Connection));
            begin
               Log.Error ("Query failed: '{0}'", Expanded_Query);
               Log.Error ("  with error: '{0}'", Message);
               raise SQL_Error with "SQL query failed: " & Message;
            end;
         end if;
      end;

      Stmt.Result     := Mysql_Store_Result (Stmt.Connection);
      if Stmt.Result /= null then
         Stmt.Max_Column := Natural (Mysql_Num_Fields (Stmt.Result));
         Stmt.Status     := HAS_ROW;
         Stmt.Row        := Mysql_Fetch_Row (Stmt.Result);
      else
         Stmt.Max_Column := 0;
         Stmt.Status     := DONE;
      end if;
   end Execute;

   --  ------------------------------
   --  Get the number of rows returned by the query
   --  ------------------------------
   overriding
   function Get_Row_Count (Query : in Mysql_Query_Statement) return Natural is
   begin
      if Query.Result = null then
         return 0;
      end if;

      return Natural (Mysql_Num_Rows (Query.Result));
   end Get_Row_Count;

   --  ------------------------------
   --  Returns True if there is more data (row) to fetch
   --  ------------------------------
   overriding
   function Has_Elements (Query : in Mysql_Query_Statement) return Boolean is
   begin
      if Query.Status = DONE
        or else Query.Status = ERROR
        or else Query.Row = null
      then
         return False;
      end if;
      return True;
   end Has_Elements;

   --  ------------------------------
   --  Fetch the next row
   --  ------------------------------
   overriding
   procedure Next (Query : in out Mysql_Query_Statement) is
   begin
      if Query.Result /= null then
         Query.Row := Mysql_Fetch_Row (Query.Result);
      else
         Log.Warn ("Next has no more element in the result set.");
      end if;
   end Next;

   --  ------------------------------
   --  Returns true if the column <b>Column</b> is null.
   --  ------------------------------
   overriding
   function Is_Null (Query  : in Mysql_Query_Statement;
                     Column : in Natural) return Boolean is
      Field : constant chars_ptr := Query.Get_Field (Column);
   begin
      return Field = null;
   end Is_Null;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Int64 (Query  : Mysql_Query_Statement;
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
   function Get_Long_Float (Query  : Mysql_Query_Statement;
                            Column : Natural) return Long_Float is
      Field  : constant chars_ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         raise Invalid_Type with "NULL cannot be converted to Long_Float";
      else
         return Get_Long_Float (Field);
      end if;
   end Get_Long_Float;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Unbounded_String (Query  : Mysql_Query_Statement;
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
   function Get_String (Query  : Mysql_Query_Statement;
                        Column : Natural) return String is
   begin
      return To_String (Query.Get_Unbounded_String (Column));
   end Get_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   --  ------------------------------
   overriding
   function Get_Blob (Query  : in Mysql_Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref is
      Field   : constant chars_ptr := Query.Get_Field (Column);
   begin
      if Field /= null then
         return Get_Blob (Size => Query.Get_Field_Length (Column),
                          Data => Field);
      else
         --  A Blob_Ref can hold a NULL so this is ok to return a Null_Blob value.
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
   function Get_Time (Query  : Mysql_Query_Statement;
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
   function Get_Column_Type (Query  : Mysql_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type is
      Field : MYSQL_FIELD;
   begin
      if Query.Result = null then
         raise Invalid_Statement with "No statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      end if;

      Field := Mysql_Fetch_Field_Direct (Query.Result, MYSQL_FIELD_OFFSET (Column));
      if Field = null then
         return ADO.Schemas.T_NULL;
      end if;
      case Field.C_Type is
         when MYSQL_TYPE_DECIMAL | MYSQL_TYPE_NEWDECIMAL =>
            return ADO.Schemas.T_DECIMAL;

         when MYSQL_TYPE_TINY =>
            return ADO.Schemas.T_TINYINT;

         when MYSQL_TYPE_SHORT =>
            return ADO.Schemas.T_SMALLINT;

         when MYSQL_TYPE_LONG | MYSQL_TYPE_INT24 =>
            return ADO.Schemas.T_LONG_INTEGER;

         when MYSQL_TYPE_FLOAT =>
            return ADO.Schemas.T_FLOAT;

         when MYSQL_TYPE_DOUBLE =>
            return ADO.Schemas.T_DOUBLE;

         when MYSQL_TYPE_NULL =>
            return ADO.Schemas.T_UNKNOWN;

         when MYSQL_TYPE_TIMESTAMP =>
            return ADO.Schemas.T_TIMESTAMP;

         when MYSQL_TYPE_LONGLONG =>
            return ADO.Schemas.T_LONG_INTEGER;

         when MYSQL_TYPE_DATE | MYSQL_TYPE_NEWDATE =>
            return ADO.Schemas.T_DATE;

         when MYSQL_TYPE_TIME =>
            return ADO.Schemas.T_TIME;

         when MYSQL_TYPE_DATETIME =>
            return ADO.Schemas.T_DATE_TIME;

         when MYSQL_TYPE_YEAR =>
            return ADO.Schemas.T_YEAR;

         when MYSQL_TYPE_VARCHAR | MYSQL_TYPE_VAR_STRING | MYSQL_TYPE_STRING =>
            return ADO.Schemas.T_VARCHAR;

         when MYSQL_TYPE_BIT =>
            return ADO.Schemas.T_BOOLEAN;

         when MYSQL_TYPE_ENUM =>
            return ADO.Schemas.T_ENUM;

         when MYSQL_TYPE_SET =>
            return ADO.Schemas.T_SET;

         when MYSQL_TYPE_TINY_BLOB |
              MYSQL_TYPE_MEDIUM_BLOB |
              MYSQL_TYPE_LONG_BLOB |
              MYSQL_TYPE_BLOB =>
            return ADO.Schemas.T_BLOB;

         when others =>
            return ADO.Schemas.T_UNKNOWN;
      end case;
   end Get_Column_Type;

   --  ------------------------------
   --  Get the column name
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Column_Name (Query  : in Mysql_Query_Statement;
                             Column : in Natural)
                             return String is
      use type Interfaces.C.Strings.chars_ptr;

      Field : MYSQL_FIELD;
   begin
      if Query.Result = null then
         raise Invalid_Statement with "No statement";
      end if;
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      end if;

      Field := Mysql_Fetch_Field_Direct (Query.Result, MYSQL_FIELD_OFFSET (Column));
      if Field = null then
         raise Invalid_Column with "Invalid column: " & Natural'Image (Column);
      end if;
      if Field.name = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Field.name);
      end if;
   end Get_Column_Name;

   --  ------------------------------
   --  Get the number of columns in the result.
   --  ------------------------------
   overriding
   function Get_Column_Count (Query  : in Mysql_Query_Statement)
                              return Natural is
   begin
      if Query.Result = null then
         raise Invalid_Statement with "No statement";
      end if;
      return Query.Max_Column;
   end Get_Column_Count;

   overriding
   procedure Finalize (Query : in out Mysql_Query_Statement) is
   begin
      if Query.Result /= null then
         Mysql_Free_Result (Query.Result);
         Query.Result := null;
      end if;
   end Finalize;

   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
      Result : constant Mysql_Query_Statement_Access := new Mysql_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Mysql_Dialect'Access);
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

   function Create_Statement (Database : in Mysql_Access;
                              Query    : in String)
                              return Query_Statement_Access is
      Result : constant Mysql_Query_Statement_Access := new Mysql_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Mysql_Dialect'Access);
      Append (Query => Result.all, SQL => Query);
      return Result.all'Access;
   end Create_Statement;

end ADO.Statements.Mysql;
