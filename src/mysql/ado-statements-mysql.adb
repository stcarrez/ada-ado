-----------------------------------------------------------------------
--  ADO.Statements.Mysql -- MySQL Statements
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

with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Calendar.Formatting;
with Util.Log;
with Util.Log.Loggers;
with System.Storage_Elements;
with Interfaces.C;
with Mysql.Com;  use Mysql.Com;
with ADO.C;
package body ADO.Statements.Mysql is

   use Util.Log;
   use Ada.Calendar;
   use System.Storage_Elements;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Statements.Mysql");

   use Interfaces.C;
   use type ADO.Schemas.Class_Mapping_Access;

   function Execute (Connection : in Mysql_Access;
                     Query      : in String) return int is
      Sql : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Query);
   begin
      Log.Debug ("Execute {0}", Query);

      --  Execute the query
      return Mysql_Query (Connection, ADO.C.To_C (Sql));
   end Execute;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Mysql_Delete_Statement) is
      Result    : int;
   begin
      ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => "DELETE FROM ");
      ADO.SQL.Append_Name (Target => Stmt.Query.SQL, Name => Stmt.Table.Table.all);
      if Stmt.Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.Query.SQL, SQL => Stmt.Query.Get_Filter);
      end if;

      declare
         Sql_Query : constant String := Stmt.Query.Expand;
      begin
         Result := Execute (Stmt.Connection, Sql_Query);
      end;
   end Execute;

   --  Create the delete statement
   overriding
   function Create_Statement (Proxy : Delete_Statement_Access) return Mysql_Delete_Statement is
   begin
      return Result : Mysql_Delete_Statement;
   end Create_Statement;

   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
      Result : constant Mysql_Delete_Statement_Access := new Mysql_Delete_Statement;
   begin
--        Result.SQL := To_Unbounded_String (Query);
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Query      := Result.Delete_Query'Access;
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

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Mysql_Update_Statement;
                      Result : out Integer) is
   begin
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => "UPDATE ");
      ADO.SQL.Append_Name (Target => Stmt.This_Query.SQL, Name => Stmt.Table.Table.all);
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " SET ");
      ADO.SQL.Append_Fields (Update => Stmt.This_Query);
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
      end if;

      declare
         Sql_Query : constant String := Stmt.This_Query.Expand;
         Res       : Int;
         Res2  : my_ulonglong;
      begin
         Res := Execute (Stmt.Connection, Sql_Query);

         Res2 := Mysql_Affected_Rows (Stmt.Connection);
         Log.Info ("Update: {0}", my_ulonglong'Image (Res2));
         Result := Integer (Res2);
      end;
   end Execute;

   --  Create an update statement
   overriding
   function Create_Statement (Proxy : Update_Statement_Access) return Mysql_Update_Statement is
   begin
      return Result : Mysql_Update_Statement;
   end Create_Statement;

   --  Create an update statement.
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
      Result : constant Mysql_Update_Statement_Access := new Mysql_Update_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Update     := Result.This_Query'Access;
      Result.Query      := Result.This_Query'Access;
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Insert statement
   --  ------------------------------

   --  Execute the query
   overriding
   procedure Execute (Stmt   : in out Mysql_Insert_Statement;
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
         Sql_Query : constant String := Stmt.This_Query.Expand;
         Res       : Int;
      begin
         Res := Execute (Stmt.Connection, Sql_Query);
         Result := 1;
      end;
--        Result := Integer (Mysql_Affected_Rows (Stmt.Connection));
   end Execute;

   --  Create the insert statement.
   overriding
   function Create_Statement (Proxy : Update_Statement_Access) return Mysql_Insert_Statement is
   begin
      return Result : Mysql_Insert_Statement;
   end Create_Statement;

   --  Create an insert statement.
   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
      Result : constant Mysql_Insert_Statement_Access := new Mysql_Insert_Statement;
   begin
      Result.Connection := Database;
      Result.Table  := Table;
      Result.Update := Result.This_Query'Access;
--        Result.Proxy  := Result.all'Access;
      ADO.SQL.Set_Insert_Mode (Result.This_Query);
      return Result.all'Access;
   end Create_Statement;

   --  ------------------------------
   --  Query statement for MySQL
   --  ------------------------------

   function To_System_Access is
     new Ada.Unchecked_Conversion (System.Address, System_Access);

   function To_Address is
     new Ada.Unchecked_Conversion (System_Access, System.Address);

   function To_Array_Chars_Ptr is
     new Ada.Unchecked_Conversion (System.Address, MYSQL_ROW);

   function "+" (Left :System_Access; Right : Size_T) return System_Access;
   pragma Inline ("+");

   function "+" (Left : System_Access; Right : Size_T) return System_Access is
   begin
      return To_System_Access (To_Address (Left) + Storage_Offset (Right));
   end "+";

   --  ------------------------------
   --  Get a column field address.
   --  If the query was not executed, raises Invalid_Statement
   --  If the column is out of bound, raises Constraint_Error
   --  ------------------------------
   function Get_Field (Query  : Mysql_Query_Statement'Class;
                       Column : Natural) return Chars_Ptr is
      use System;

      Result : Chars_Ptr;
      R : System_Access;
   begin
      if Query.Row = null then
         raise Invalid_Statement with "Null statement";
      end if;
      if Column > Query.Max_Column then
         raise Constraint_Error with "Invalid column";
      end if;
      R := Query.Row + Size_T (Column * (R'Size / 8));
      return To_Chars_Ptr (R.all);
   end Get_Field;

   --  Execute the query
   overriding
   procedure Execute (Stmt : in out Mysql_Query_Statement) is
      use System;

      Result : Int;
   begin
      if Stmt.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => Stmt.This_Query.Get_Filter);
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
               Log.Info ("         SQL: '{0}'", Expanded_Query);
               raise Invalid_Statement with "Query failed: " & Message;
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

   --  Get the number of rows returned by the query
   overriding
   function Get_Row_Count (Query : in Mysql_Query_Statement) return Natural is
   begin
      if Query.Result = null then
         return 0;
      end if;

      return Natural (Mysql_Num_Rows (Query.Result));
   end Get_Row_Count;

   --  Returns True if there is more data (row) to fetch
   overriding
   function Has_Elements (Query : in Mysql_Query_Statement) return Boolean is
      use System;
   begin
      if Query.Status = DONE
        or else Query.Status = ERROR
        or else Query.Row = null then
         return False;
      end if;
      return True;
   end Has_Elements;


   --  Fetch the next row
   overriding
   procedure Next (Query : in out Mysql_Query_Statement) is
   begin
      if Query.Result /= null then
         Query.Row := Mysql_Fetch_Row (Query.Result);
      else
         Log.Warn ("Next has no more element in the result set.");
      end if;
   end Next;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Int64 (Query  : Mysql_Query_Statement;
                       Column : Natural) return Int64 is
      Field  : Chars_Ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         return 0;
      else
         return Get_Int64 (Field);
      end if;
   end Get_Int64;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Unbounded_String (Query  : Mysql_Query_Statement;
                                  Column : Natural) return Unbounded_String is
      Field  : Chars_Ptr := Query.Get_Field (Column);
   begin
      if Field = null then
         return Null_Unbounded_String;
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

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_String (Query  : Mysql_Query_Statement;
                        Column : Natural) return String is
   begin
      return To_String (Query.Get_Unbounded_String (Column));
   end Get_String;

   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   function Get_Time (Query  : Mysql_Query_Statement;
                      Column : Natural) return Ada.Calendar.Time is
   begin
      return Clock;
   end Get_Time;

   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Column_Type (Query  : Mysql_Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type is
      Old   : MYSQL_FIELD_OFFSET;
      Field : MYSQL_FIELD;
   begin
      if Query.Result = null then
         raise Invalid_Statement with "No statement";
      end if;
      if Column > Query.Max_Column then
         raise Constraint_Error with "Invalid column: " & Natural'Image (Column);
      end if;

      Old := Mysql_Field_Seek (Query.Result,
                               MYSQL_FIELD_OFFSET (Column - 1));
      pragma Unreferenced (Old);

      Field := Mysql_Fetch_Fields (Query.Result);
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

   overriding
   procedure Finalize (Query : in out Mysql_Query_Statement) is
      use System;
   begin
      if Query.Result /= null then
         Mysql_Free_Result (Query.Result);
         Query.Result := null;
      end if;
   end Finalize;

   --  Create the query statement.
   overriding
   function Create_Statement (Proxy :Query_Statement_Access) return Mysql_Query_Statement is
   begin
      return Result : Mysql_Query_Statement;
   end Create_Statement;

   function Create_Statement (Database : in Mysql_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
      use type ADO.Schemas.Class_Mapping_Access;
      Result : constant Mysql_Query_Statement_Access := new Mysql_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
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
      return Result.all'Access;
   end Create_Statement;

--   procedure Free_Param (Arg1 : System.Address) is
--      S : Strings.chars_ptr := To_Chars_Ptr (Arg1);
--   begin
--      Strings.Free (S);
--   end Free_Param;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
--     overriding
--     procedure Execute (Query  : in out Mysql_Statement;
--                        Result : out Integer) is
--     begin
--        Statement'Class (Query).Execute;
--        Result := Integer (mysql_affected_rows (Query.Connection));
--     end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
--     overriding
--     procedure Execute (Query : in out Mysql_Statement;
--                        Id    : out Identifier) is
--     begin
--        Statement'Class (Query).Execute;
--        Id := Identifier (mysql_insert_id (Query.Connection));
--     end Execute;
--
--
--     function Get_Time (Iter : Mysql_Statement; Column : Natural) return Time is
--     begin
--        return Clock;
--     end Get_Time;

end ADO.Statements.Mysql;
