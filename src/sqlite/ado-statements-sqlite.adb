-----------------------------------------------------------------------
--  ADO Databases -- Database connections
--  Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of ADO.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330,
--  Boston, MA 02111-1307, USA.
-----------------------------------------------------------------------
with Sqlite3_H;

with Interfaces.C.Strings;

with Util.Log;
with Util.Log.Loggers;

package body ADO.Statements.Sqlite is

   use Util.Log;
   use Ada.Calendar;

   use type ADO.Schemas.Class_Mapping_Access;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Statements.Sqlite");

   procedure Execute (Connection : in ADO.Drivers.Sqlite.Sqlite_Access;
                      SQL        : in String;
                      Result     : out int;
                      Stmt       : out System.Address);

   --  Releases the sqlite statement
   procedure Release_Stmt (Connection : in ADO.Drivers.Sqlite.Sqlite_Access;
                           Stmt       : in out System.Address);

   procedure Prepare (Stmt  : in out Sqlite_Query_Statement;
                      Query : in String);

   --  ------------------------------
   --  Releases the sqlite statement
   --  ------------------------------
   procedure Release_Stmt (Connection : in ADO.Drivers.Sqlite.Sqlite_Access;
                           Stmt       : in out System.Address) is
      use System;

      Result : int;
   begin
      if Stmt /= System.Null_Address then
         Result := Sqlite3_H.sqlite3_reset (Stmt);
         ADO.Drivers.Sqlite.Check_Error (Connection, Result);

         Result := Sqlite3_H.sqlite3_finalize (Stmt);
         ADO.Drivers.Sqlite.Check_Error (Connection, Result);

         Stmt := System.Null_Address;
      end if;
   end Release_Stmt;

   --  ------------------------------
   --  Delete statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Sqlite_Delete_Statement;
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
         Sql_Query : constant String := Stmt.Query.Expand;
         Handle    : aliased System.Address;
         Res       : int;
      begin
         Execute (Connection => Stmt.Connection,
                  SQL        => Sql_Query,
                  Result     => Res,
                  Stmt       => Handle);
         ADO.Drivers.Sqlite.Check_Error (Stmt.Connection, Res);

         Release_Stmt (Stmt.Connection, Handle);
         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
      end;
   end Execute;

   --  ------------------------------
   --  Create the delete statement
   --  ------------------------------
   function Create_Statement (Database : in ADO.Drivers.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
      Result : constant Sqlite_Delete_Statement_Access := new Sqlite_Delete_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Query      := Result.Delete_Query'Access;
      return Result.all'Access;
   end Create_Statement;

   procedure Execute (Connection : in ADO.Drivers.Sqlite.Sqlite_Access;
                      SQL        : in String;
                      Result     : out int;
                      Stmt       : out System.Address) is
      ZSql   : Strings.chars_ptr := Strings.New_String (SQL);
      Handle : aliased System.Address;
   begin
      Log.Debug ("Execute: {0}", SQL);

      Result := Sqlite3_H.sqlite3_prepare_v2 (db     => Connection,
                                              zSql   => ZSql,
                                              nByte  => int (SQL'Length + 1),
                                              ppStmt => Handle'Address,
                                              pzTail => System.Null_Address);
      Strings.Free (ZSql);
      Stmt := Handle;
      if Result /= Sqlite3_H.SQLITE_OK then
         ADO.Drivers.Sqlite.Check_Error (Connection, Result);
         return;
      end if;

      Result := Sqlite3_H.sqlite3_step (Handle);
   end Execute;

   --  ------------------------------
   --  Update statement
   --  ------------------------------

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt : in out Sqlite_Update_Statement) is
      Result : Integer;
   begin
      Sqlite_Update_Statement'Class (Stmt).Execute (Result);
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Sqlite_Update_Statement;
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
         Sql_Query : constant String := Stmt.This_Query.Expand;
         Handle    : aliased System.Address;
         Res       : int;
      begin
         Execute (Connection => Stmt.Connection,
                  SQL        => Sql_Query,
                  Result     => Res,
                  Stmt       => Handle);
         ADO.Drivers.Sqlite.Check_Error (Stmt.Connection, Res);

         Release_Stmt (Stmt.Connection, Handle);
         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
      end;
   end Execute;

   --  ------------------------------
   --  Create an update statement
   --  ------------------------------
   function Create_Statement (Database : in ADO.Drivers.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
      Result : constant Sqlite_Update_Statement_Access := new Sqlite_Update_Statement;
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

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Stmt   : in out Sqlite_Insert_Statement;
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
         Handle    : aliased System.Address;
         Res       : int;
      begin
         Execute (Connection => Stmt.Connection,
                  SQL        => Sql_Query,
                  Result     => Res,
                  Stmt       => Handle);
         ADO.Drivers.Sqlite.Check_Error (Stmt.Connection, Res);

         Release_Stmt (Stmt.Connection, Handle);
         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
      end;
   end Execute;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   function Create_Statement (Database : in ADO.Drivers.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
      Result : constant Sqlite_Insert_Statement_Access := new Sqlite_Insert_Statement;
   begin
      Result.Connection := Database;
      Result.Table  := Table;
      Result.Update := Result.This_Query'Access;
      ADO.SQL.Set_Insert_Mode (Result.This_Query);
      return Result.all'Access;
   end Create_Statement;

   procedure Prepare (Stmt  : in out Sqlite_Query_Statement;
                      Query : in String) is
      Sql    : Strings.chars_ptr := Strings.New_String (Query);
      Result : int;
      Handle : aliased System.Address;
   begin
      Result := Sqlite3_H.sqlite3_prepare_v2 (db     => Stmt.Connection,
                                              zSql   => Sql,
                                              nByte  => int (Query'Length + 1),
                                              ppStmt => Handle'Address,
                                              pzTail => System.Null_Address);
      Strings.Free (Sql);
      if Result = Sqlite3_H.SQLITE_OK then
         Stmt.Stmt := Handle;
      else
         ADO.Drivers.Sqlite.Check_Error (Stmt.Connection, Result);
      end if;

   end Prepare;

   --  Execute the query
   procedure Execute (Query : in out Sqlite_Query_Statement) is
      use Sqlite3_H;
      use System;

      Result : int;
   begin
      if Query.This_Query.Has_Join then
         ADO.SQL.Append (Target => Query.This_Query.SQL, SQL => " ");
         ADO.SQL.Append (Target => Query.This_Query.SQL, SQL => Query.This_Query.Get_Join);
      end if;
      if Query.This_Query.Has_Filter then
         ADO.SQL.Append (Target => Query.This_Query.SQL, SQL => " WHERE ");
         ADO.SQL.Append (Target => Query.This_Query.SQL, SQL => Query.This_Query.Get_Filter);
      end if;

      declare
         Expanded_Query : constant String := Query.Query.Expand;
      begin
         --  Execute the query
         Prepare (Query, Expanded_Query);

         Log.Debug ("Execute: {0}", Expanded_Query);

         Result := Sqlite3_H.sqlite3_step (Query.Stmt);
         if Result = Sqlite3_H.SQLITE_ROW then
            Query.Status := HAS_ROW;

         elsif Result = Sqlite3_H.SQLITE_DONE then
            Query.Status := DONE;

         else
            Query.Status := ERROR;
            declare
               Error   : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errmsg (Query.Connection);
               Message : constant String := Strings.Value (Error);
            begin
               Log.Error ("Query failed: '{0}'", Expanded_Query);
               Log.Error ("  with error: '{0}'", Message);
               Log.Info ("         SQL: '{0}'", Expanded_Query);
               raise Invalid_Statement with "Query failed: " & Message;
            end;
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Returns True if there is more data (row) to fetch
   --  ------------------------------
   function Has_Elements (Query : in Sqlite_Query_Statement) return Boolean is
      use type System.Address;
   begin
      return Query.Stmt /= System.Null_Address and then Query.Status = HAS_ROW;
   end Has_Elements;

   --  ------------------------------
   --  Fetch the next element
   --  ------------------------------
   overriding
   procedure Next (Query : in out Sqlite_Query_Statement) is
      use type System.Address;

      Result : int;
   begin
      if Query.Stmt = System.Null_Address then
         Query.Status := ERROR;
      else
         Result := Sqlite3_H.sqlite3_step (Query.Stmt);
         if Result = Sqlite3_H.SQLITE_ROW then
            Query.Status := HAS_ROW;

         elsif Result = Sqlite3_H.SQLITE_DONE then
            Query.Status := DONE;

         else
            ADO.Drivers.Sqlite.Check_Error (Query.Connection, Result);
            Query.Status := ERROR;
         end if;
      end if;
   end Next;

   --  ------------------------------
   --  Returns true if the column <b>Column</b> is null.
   --  ------------------------------
   overriding
   function Is_Null (Query  : in Sqlite_Query_Statement;
                     Column : in Natural) return Boolean is
      Res : constant int := Sqlite3_H.sqlite3_column_type (Query.Stmt, int (Column));
   begin
      return Res = Sqlite3_H.SQLITE_NULL;
   end Is_Null;

   --  Get the number of rows returned by the query
   function Get_Row_Count (Query : in Sqlite_Query_Statement) return Natural is
   begin
      return 0;
   end Get_Row_Count;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Int64 (Query  : in Sqlite_Query_Statement;
                       Column : in Natural) return Int64 is
      Result : constant Sqlite3_H.sqlite_int64
        := Sqlite3_H.sqlite3_column_int64 (Query.Stmt, int (Column));
   begin
      return Int64 (Result);
   end Get_Int64;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Unbounded_String (Query  : in Sqlite_Query_Statement;
                                  Column : in Natural) return Unbounded_String is
      use type Strings.chars_ptr;

      Text : constant Strings.chars_ptr
        := Sqlite3_H.sqlite3_column_text (Query.Stmt, int (Column));
   begin
      if Text = Strings.Null_Ptr then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (Interfaces.C.Strings.Value (Text));
      end if;
   end Get_Unbounded_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_String (Query  : Sqlite_Query_Statement;
                        Column : Natural) return String is
      use type Strings.chars_ptr;

      Text : constant Strings.chars_ptr
        := Sqlite3_H.sqlite3_column_text (Query.Stmt, int (Column));
   begin
      if Text = Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Text);
      end if;
   end Get_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Time (Query  : in Sqlite_Query_Statement;
                      Column : in Natural) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Clock;
   end Get_Time;

   --  ------------------------------
   --  Get the column type
   --  ------------------------------
   overriding
   function Get_Column_Type (Query  : in Sqlite_Query_Statement;
                             Column : in Natural)
                             return ADO.Schemas.Column_Type is
      Res : constant int := Sqlite3_H.sqlite3_column_type (Query.Stmt, int (Column));
   begin
      case Res is
         when Sqlite3_H.SQLITE_NULL =>
            return ADO.Schemas.T_NULL;

         when Sqlite3_H.SQLITE_INTEGER =>
            return ADO.Schemas.T_INTEGER;

         when Sqlite3_H.SQLITE_FLOAT =>
            return ADO.Schemas.T_FLOAT;

         when Sqlite3_H.SQLITE_TEXT =>
            return ADO.Schemas.T_VARCHAR;

         when Sqlite3_H.SQLITE_BLOB =>
            return ADO.Schemas.T_BLOB;

         when others =>
            return ADO.Schemas.T_UNKNOWN;

      end case;
   end Get_Column_Type;

   --  ------------------------------
   --  Deletes the query statement.
   --  ------------------------------
   overriding
   procedure Finalize (Query : in out Sqlite_Query_Statement) is
   begin
      Release_Stmt (Query.Connection, Query.Stmt);
   end Finalize;

   --  ------------------------------
   --  Create the query statement.
   --  ------------------------------
   function Create_Statement (Database : in ADO.Drivers.Sqlite.Sqlite_Access;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
      Result : constant Sqlite_Query_Statement_Access := new Sqlite_Query_Statement;
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

   --  ------------------------------
   --  Create the query statement.
   --  ------------------------------
   function Create_Statement (Database : in ADO.Drivers.Sqlite.Sqlite_Access;
                              Query    : in String)
                              return Query_Statement_Access is
      pragma Unreferenced (Query);

      Result : constant Sqlite_Query_Statement_Access := new Sqlite_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      return Result.all'Access;
   end Create_Statement;

end ADO.Statements.Sqlite;
