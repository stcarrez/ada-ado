-----------------------------------------------------------------------
--  ado-statements-sqlite -- SQLite database statements
--  Copyright (C) 2009 - 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Sqlite3_H.Perfect_Hash;

with Interfaces.C.Strings;

with Util.Log;
with Util.Log.Loggers;

with ADO.Sessions;
with ADO.Dialects;
package body ADO.Statements.Sqlite is

   use type ADO.Schemas.Class_Mapping_Access;
   use type ADO.Schemas.Column_Index;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Statements.Sqlite");

   type Dialect is new ADO.Dialects.Dialect with null record;

   --  Check if the string is a reserved keyword.
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean;

   --  Get the quote character to escape an identifier.
   overriding
   function Get_Identifier_Quote (D : in Dialect) return Character;

   --  Append the item in the buffer escaping some characters if necessary
   overriding
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref);

   Sqlite_Dialect : aliased Dialect;

   function Execute (Connection : access ADO.Connections.Sqlite.Sqlite3;
                     SQL        : in String) return access Sqlite3_H.sqlite3_stmt;

   --  Releases the sqlite statement
   procedure Release_Stmt (Connection : access ADO.Connections.Sqlite.Sqlite3;
                           Stmt       : access Sqlite3_H.sqlite3_stmt);

   procedure Prepare (Stmt  : in out Sqlite_Query_Statement;
                      Query : in String);

   --  ------------------------------
   --  Check if the string is a reserved keyword.
   --  ------------------------------
   overriding
   function Is_Reserved (D    : in Dialect;
                         Name : in String) return Boolean is
      pragma Unreferenced (D);
   begin
      return Sqlite3_H.Perfect_Hash.Is_Keyword (Name);
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

            when Character'Pos (''') =>
               Append (Buffer, ''');
               Append (Buffer, Character'Val (C));

            when others =>
               Append (Buffer, Character'Val (C));
         end case;
      end loop;
      Append (Buffer, ''');
   end Escape_Sql;

   --  ------------------------------
   --  Check for an error after executing a sqlite statement.
   --  ------------------------------
   procedure Check_Error (Connection : access ADO.Connections.Sqlite.Sqlite3;
                          SQL        : in String;
                          Result     : in int) is
   begin
      if Result /= Sqlite3_H.SQLITE_OK
        and then Result /= Sqlite3_H.SQLITE_DONE
        and then Result /= Sqlite3_H.SQLITE_ROW
      then
         if Result = Sqlite3_H.SQLITE_BUSY then
            Log.Info ("SQLite busy, query failed: '{0}'", SQL);
            raise ADO.Objects.LAZY_LOCK;
         end if;
         declare
            Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errmsg (Connection);
            Msg   : constant String := Strings.Value (Error);
         begin
            if SQL'Length > 0 then
               Log.Error ("Query failed: '{0}'", SQL);
            end if;
            Log.Error ("Error {0}: {1}", int'Image (Result), Msg);
            raise ADO.Statements.SQL_Error with "SQL error: " & Msg;
         end;
      end if;
   end Check_Error;

   function Execute (Connection : access ADO.Connections.Sqlite.Sqlite3;
                     SQL        : in String) return access Sqlite3_H.sqlite3_stmt is

      function sqlite3_prepare_v2 (db : access Sqlite3_H.sqlite3;
                                   zSql : String;
                                   nByte : int;
                                   ppStmt : System.Address;
                                   pzTail : System.Address) return int;
      pragma Import (C, sqlite3_prepare_v2, "sqlite3_prepare_v2");

      Handle : aliased access Sqlite3_H.sqlite3_stmt;
      Result : int;
   begin
      Log.Debug ("Execute: {0}", SQL);

      Result := sqlite3_prepare_v2 (db     => Connection,
                                    zSql   => SQL,
                                    nByte  => int (SQL'Length),
                                    ppStmt => Handle'Address,
                                    pzTail => System.Null_Address);
      if Result /= Sqlite3_H.SQLITE_OK then
         Check_Error (Connection, SQL, Result);
      end if;

      return Handle;
   end Execute;

   --  ------------------------------
   --  Execute SQL statement.
   --  ------------------------------
   procedure Execute (Connection : access ADO.Connections.Sqlite.Sqlite3;
                      SQL        : in String) is
      Handle    : access Sqlite3_H.sqlite3_stmt;
      Res       : int;
   begin
      Handle := Execute (Connection => Connection,
                         SQL        => SQL);

      Res := Sqlite3_H.sqlite3_step (Handle);
      Release_Stmt (Connection, Handle);
      Check_Error (Connection, SQL, Res);
   end Execute;

   --  ------------------------------
   --  Releases the sqlite statement
   --  ------------------------------
   procedure Release_Stmt (Connection : access ADO.Connections.Sqlite.Sqlite3;
                           Stmt       : access Sqlite3_H.sqlite3_stmt) is
      Result : int;
      pragma Unreferenced (Connection, Result);
   begin
      Result := Sqlite3_H.sqlite3_finalize (Stmt);
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
         Handle    : access Sqlite3_H.sqlite3_stmt;
         Res       : int;
      begin
         Handle := Execute (Connection => Stmt.Connection,
                            SQL        => Sql_Query);

         Res := Sqlite3_H.sqlite3_step (Handle);
         Release_Stmt (Stmt.Connection, Handle);
         Check_Error (Stmt.Connection, Sql_Query, Res);

         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Deleted{0} rows", Natural'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create the delete statement
   --  ------------------------------
   function Create_Statement (Database : access ADO.Connections.Sqlite.Sqlite3;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
      Result : constant Sqlite_Delete_Statement_Access := new Sqlite_Delete_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Query      := Result.Delete_Query'Access;
      Result.Delete_Query.Set_Dialect (Sqlite_Dialect'Access);
      return Result.all'Access;
   end Create_Statement;

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
      Allow_As : constant Boolean := Sqlite3_H.sqlite3_libversion_number > 3025000;
   begin
      if Stmt.Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => "UPDATE ");
      ADO.SQL.Append_Name (Target => Stmt.This_Query.SQL, Name => Stmt.Table.Table.all);
      if Allow_As then
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " AS o SET ");
      else
         ADO.SQL.Append (Target => Stmt.This_Query.SQL, SQL => " SET ");
      end if;
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
         Handle    : access Sqlite3_H.sqlite3_stmt;
         Res       : int;
      begin
         Handle := Execute (Connection => Stmt.Connection,
                            SQL        => Sql_Query);

         Res := Sqlite3_H.sqlite3_step (Handle);
         Release_Stmt (Stmt.Connection, Handle);
         Check_Error (Stmt.Connection, Sql_Query, Res);

         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Updated{0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an update statement
   --  ------------------------------
   function Create_Statement (Database : access ADO.Connections.Sqlite.Sqlite3;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
      Result : constant Sqlite_Update_Statement_Access := new Sqlite_Update_Statement;
   begin
      Result.Connection := Database;
      Result.Table      := Table;
      Result.Update     := Result.This_Query'Access;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Sqlite_Dialect'Access);
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
         Handle    : access Sqlite3_H.sqlite3_stmt;
         Res       : int;
      begin
         Handle := Execute (Connection => Stmt.Connection,
                            SQL        => Sql_Query);

         Res := Sqlite3_H.sqlite3_step (Handle);
         Release_Stmt (Stmt.Connection, Handle);
         Check_Error (Stmt.Connection, Sql_Query, Res);

         Result := Natural (Sqlite3_H.sqlite3_changes (Stmt.Connection));
         if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
            Log.Debug ("Inserted{0} rows", Integer'Image (Result));
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   function Create_Statement (Database : access ADO.Connections.Sqlite.Sqlite3;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
      Result : constant Sqlite_Insert_Statement_Access := new Sqlite_Insert_Statement;
   begin
      Result.Connection := Database;
      Result.Table  := Table;
      Result.Update := Result.This_Query'Access;
      Result.Query  := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Sqlite_Dialect'Access);
      ADO.SQL.Set_Insert_Mode (Result.This_Query);
      return Result.all'Access;
   end Create_Statement;

   procedure Prepare (Stmt  : in out Sqlite_Query_Statement;
                      Query : in String) is

      function sqlite3_prepare_v2 (db : access Sqlite3_H.sqlite3;
                                   zSql : String;
                                   nByte : int;
                                   ppStmt : System.Address;
                                   pzTail : System.Address) return int;
      pragma Import (C, sqlite3_prepare_v2, "sqlite3_prepare_v2");

      Result : int;
      Handle : aliased access Sqlite3_H.sqlite3_stmt;
   begin
      Log.Debug ("Execute: {0}", Query);

      if Stmt.Stmt /= null then
         Release_Stmt (Stmt.Connection, Stmt.Stmt);
         Stmt.Stmt := null;
      end if;
      Result := sqlite3_prepare_v2 (db     => Stmt.Connection,
                                    zSql   => Query,
                                    nByte  => Query'Length,
                                    ppStmt => Handle'Address,
                                    pzTail => System.Null_Address);
      Check_Error (Stmt.Connection, Query, Result);
      Stmt.Stmt := Handle;
   end Prepare;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Query : in out Sqlite_Query_Statement) is
      Result : int;
   begin
      if Query.Connection = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
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

         Result := Sqlite3_H.sqlite3_step (Query.Stmt);
         if Result = Sqlite3_H.SQLITE_ROW then
            Query.Status := HAS_ROW;
            Query.Max_Column := Natural (Sqlite3_H.sqlite3_column_count (Query.Stmt));

         elsif Result = Sqlite3_H.SQLITE_DONE then
            Query.Status := DONE;
            Query.Max_Column := 0;

         else
            Query.Status := ERROR;
            Query.Max_Column := 0;
            declare
               Error   : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errmsg (Query.Connection);
               Message : constant String := Strings.Value (Error);
            begin
               Log.Error ("Query failed: '{0}'", Expanded_Query);
               Log.Error ("  with error: '{0}'", Message);
               raise ADO.Statements.SQL_Error with "Query failed: " & Message;
            end;
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Returns True if there is more data (row) to fetch
   --  ------------------------------
   overriding
   function Has_Elements (Query : in Sqlite_Query_Statement) return Boolean is
   begin
      return Query.Status = HAS_ROW;
   end Has_Elements;

   --  ------------------------------
   --  Fetch the next element
   --  ------------------------------
   overriding
   procedure Next (Query : in out Sqlite_Query_Statement) is
      Result : int;
   begin
      if Query.Stmt = null then
         Query.Status := ERROR;
      else
         Result := Sqlite3_H.sqlite3_step (Query.Stmt);
         if Result = Sqlite3_H.SQLITE_ROW then
            Query.Status := HAS_ROW;

         elsif Result = Sqlite3_H.SQLITE_DONE then
            Query.Status := DONE;

         else
            Check_Error (Query.Connection, "", Result);
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

   --  ------------------------------
   --  Get the number of rows returned by the query
   --  ------------------------------
   overriding
   function Get_Row_Count (Query : in Sqlite_Query_Statement) return Natural is
      pragma Unreferenced (Query);
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
      Result : Sqlite3_H.sqlite_int64;
      Res    : int;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Res := Sqlite3_H.sqlite3_column_type (Query.Stmt, int (Column));
      if Res = Sqlite3_H.SQLITE_NULL then
         raise Invalid_Type with "NULL cannot be converted to Integer";
      end if;
      if Res /= Sqlite3_H.SQLITE_INTEGER then
         raise Invalid_Type with "Invalid integer value";
      end if;
      Result := Sqlite3_H.sqlite3_column_int64 (Query.Stmt, int (Column));
      return Int64 (Result);
   end Get_Int64;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Long_Float</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   overriding
   function Get_Long_Float (Query  : Sqlite_Query_Statement;
                            Column : Natural) return Long_Float is
      Result : Interfaces.C.double;
      Res    : int;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Res := Sqlite3_H.sqlite3_column_type (Query.Stmt, int (Column));
      if Res = Sqlite3_H.SQLITE_NULL then
         raise Invalid_Type with "NULL cannot be converted to Long_Float";
      end if;
      if Res /= Sqlite3_H.SQLITE_FLOAT then
         raise Invalid_Type with "Invalid float value";
      end if;
      Result := Sqlite3_H.sqlite3_column_double (Query.Stmt, int (Column));
      return Long_Float (Result);
   end Get_Long_Float;

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

      Text : Strings.chars_ptr;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Text := Sqlite3_H.sqlite3_column_text (Query.Stmt, int (Column));
      if Text = Strings.Null_Ptr then
         raise Invalid_Type with "NULL cannot be converted to String";
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

      Text : Strings.chars_ptr;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Text := Sqlite3_H.sqlite3_column_text (Query.Stmt, int (Column));
      if Text = Strings.Null_Ptr then
         raise Invalid_Type with "NULL cannot be converted to String";
      else
         return Interfaces.C.Strings.Value (Text);
      end if;
   end Get_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   --  ------------------------------
   overriding
   function Get_Blob (Query  : in Sqlite_Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref is
      use type System.Address;

      Text : System.Address;
      Len  : int;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Text := Sqlite3_H.sqlite3_column_blob (Query.Stmt, int (Column));
      Len  := Sqlite3_H.sqlite3_column_bytes (Query.Stmt, int (Column));
      if Text = System.Null_Address or else Len <= 0 then
         return Null_Blob;
      else
         return Get_Blob (Size => Natural (Len),
                          Data => To_Chars_Ptr (Text));
      end if;
   end Get_Blob;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Time (Query  : in Sqlite_Query_Statement;
                      Column : in Natural) return Ada.Calendar.Time is
      use type Interfaces.C.Strings.chars_ptr;

      Text : Strings.chars_ptr;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Text := Sqlite3_H.sqlite3_column_text (Query.Stmt, int (Column));
      if Text = Strings.Null_Ptr then
         raise Invalid_Type with "NULL cannot be converted to Date";
      end if;
      return ADO.Statements.Get_Time (ADO.Statements.To_Chars_Ptr (Text));
   end Get_Time;

   --  ------------------------------
   --  Get the column type
   --  ------------------------------
   overriding
   function Get_Column_Type (Query  : in Sqlite_Query_Statement;
                             Column : in Natural)
                             return ADO.Schemas.Column_Type is
      Res : int;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Res := Sqlite3_H.sqlite3_column_type (Query.Stmt, int (Column));
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
   --  Get the column name.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   overriding
   function Get_Column_Name (Query  : in Sqlite_Query_Statement;
                             Column : in Natural)
                             return String is
      use type Interfaces.C.Strings.chars_ptr;

      Name : Interfaces.C.Strings.chars_ptr;
   begin
      if Column >= Query.Max_Column then
         raise Invalid_Column with "Invalid column" & Natural'Image (Column);
      end if;
      Name := Sqlite3_H.sqlite3_column_name (Query.Stmt, int (Column));
      if Name = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Name);
      end if;
   end Get_Column_Name;

   --  ------------------------------
   --  Get the number of columns in the result.
   --  ------------------------------
   overriding
   function Get_Column_Count (Query  : in Sqlite_Query_Statement)
                              return Natural is
   begin
      return Query.Max_Column;
   end Get_Column_Count;

   --  ------------------------------
   --  Deletes the query statement.
   --  ------------------------------
   overriding
   procedure Finalize (Query : in out Sqlite_Query_Statement) is
   begin
      if Query.Stmt /= null then
         Release_Stmt (Query.Connection, Query.Stmt);
         Query.Stmt := null;
      end if;
   end Finalize;

   --  ------------------------------
   --  Create the query statement.
   --  ------------------------------
   function Create_Statement (Database : access ADO.Connections.Sqlite.Sqlite3;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
      Result : constant Sqlite_Query_Statement_Access := new Sqlite_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Sqlite_Dialect'Access);
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
   function Create_Statement (Database : access ADO.Connections.Sqlite.Sqlite3;
                              Query    : in String)
                              return Query_Statement_Access is
      Result : constant Sqlite_Query_Statement_Access := new Sqlite_Query_Statement;
   begin
      Result.Connection := Database;
      Result.Query      := Result.This_Query'Access;
      Result.This_Query.Set_Dialect (Sqlite_Dialect'Access);
      Append (Query => Result.all, SQL => Query);
      return Result.all'Access;
   end Create_Statement;

end ADO.Statements.Sqlite;
