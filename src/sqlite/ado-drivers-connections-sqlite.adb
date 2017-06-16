-----------------------------------------------------------------------
--  ADO Sqlite Database -- SQLite Database connections
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2017 Stephane Carrez
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

with Sqlite3_H;
with Interfaces.C.Strings;

with Util.Log;
with Util.Log.Loggers;
with ADO.C;
with ADO.Statements.Sqlite;
with ADO.Schemas.Sqlite;

package body ADO.Drivers.Connections.Sqlite is

   use ADO.Statements.Sqlite;
   use Interfaces.C;

   pragma Linker_Options ("-lsqlite3");

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Databases.Sqlite");

   Driver_Name : aliased constant String := "sqlite";
   Driver      : aliased Sqlite_Driver;

   No_Callback : constant Sqlite3_H.sqlite3_callback := null;

   --  ------------------------------
   --  Get the database driver which manages this connection.
   --  ------------------------------
   overriding
   function Get_Driver (Database : in Database_Connection)
                        return Driver_Access is
      pragma Unreferenced (Database);
   begin
      return Driver'Access;
   end Get_Driver;

   --  ------------------------------
   --  Check for an error after executing a sqlite statement.
   --  ------------------------------
   procedure Check_Error (Connection : in Sqlite_Access;
                          Result     : in int) is
   begin
      if Result /= Sqlite3_H.SQLITE_OK and Result /= Sqlite3_H.SQLITE_DONE then
         declare
            Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errmsg (Connection);
            Msg   : constant String := Strings.Value (Error);
         begin
            Log.Error ("Error {0}: {1}", int'Image (Result), Msg);
         end;
      end if;
   end Check_Error;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Query => Query);
   end Create_Statement;

   --  ------------------------------
   --  Create a delete statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Create an update statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Start a transaction.
   --  ------------------------------
   procedure Begin_Transaction (Database : in out Database_Connection) is
   begin
--        Database.Execute ("begin transaction;");
      null;
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   procedure Commit (Database : in out Database_Connection) is
   begin
--        Database.Execute ("commit transaction;");
      null;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   procedure Rollback (Database : in out Database_Connection) is
   begin
--        Database.Execute ("rollback transaction;");
      null;
   end Rollback;

   procedure Sqlite3_Free (Arg1 : Strings.chars_ptr);
   pragma Import (C, sqlite3_free, "sqlite3_free");

   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String) is
      use type Strings.chars_ptr;

      SQL_Stat  : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (SQL);
      Result    : int;
      Error_Msg : Strings.chars_ptr;
   begin
      Log.Debug ("Execute: {0}", SQL);

      for Retry in 1 .. 100 loop
         Result := Sqlite3_H.sqlite3_exec (Database.Server, ADO.C.To_C (SQL_Stat), No_Callback,
                                           System.Null_Address, Error_Msg'Address);

         exit when Result /= Sqlite3_H.SQLITE_BUSY;
         delay 0.01 * Retry;
      end loop;
      Check_Error (Database.Server, Result);
      if Error_Msg /= Strings.Null_Ptr then
         Log.Error ("Error: {0}", Strings.Value (Error_Msg));
         Sqlite3_Free (Error_Msg);
      end if;

      --  Free
      --  Strings.Free (SQL_Stat);
   end Execute;

   --  ------------------------------
   --  Closes the database connection
   --  ------------------------------
   overriding
   procedure Close (Database : in out Database_Connection) is
      pragma Unreferenced (Database);
      Result : int;
   begin
      Log.Info ("Close connection");

--        if Database.Count = 1 then
--           Result := Sqlite3_H.sqlite3_close (Database.Server);
--           Database.Server := System.Null_Address;
--        end if;
      pragma Unreferenced (Result);
   end Close;

   --  ------------------------------
   --  Releases the sqlite connection if it is open
   --  ------------------------------
   overriding
   procedure Finalize (Database : in out Database_Connection) is
      Result : int;
   begin
      Log.Debug ("Release connection");

      Result := Sqlite3_H.sqlite3_close (Database.Server);
      Database.Server := System.Null_Address;
      pragma Unreferenced (Result);
   end Finalize;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      ADO.Schemas.Sqlite.Load_Schema (Database, Schema);
   end Load_Schema;

   DB : Ref.Ref;

   --  ------------------------------
   --  Initialize the database connection manager.
   --  ------------------------------
   procedure Create_Connection (D      : in out Sqlite_Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class) is
      pragma Unreferenced (D);
      use Strings;
      use type System.Address;

      Name     : constant String := To_String (Config.Database);
      Filename : Strings.chars_ptr;
      Status   : int;
      Handle   : aliased System.Address;
      Flags    : constant int := Sqlite3_H.SQLITE_OPEN_FULLMUTEX + Sqlite3_H.SQLITE_OPEN_READWRITE;

   begin
      Log.Info ("Opening database {0}", Name);

      if not DB.Is_Null then
         Result := Ref.Create (DB.Value);
         return;
      end if;
      Filename := Strings.New_String (Name);
      Status := Sqlite3_H.sqlite3_open_v2 (Filename, Handle'Access,
                                           Flags,
                                           Strings.Null_Ptr);

      Strings.Free (Filename);
      if Status /= Sqlite3_H.SQLITE_OK then
         raise DB_Error;
      end if;

      declare
         Database : constant Database_Connection_Access := new Database_Connection;

         procedure Configure (Name, Item : in Util.Properties.Value);
         function Escape (Value : in Util.Properties.Value) return String;

         function Escape (Value : in Util.Properties.Value) return String is
            S : constant String := To_String (Value);
         begin
            if S'Length > 0 and then S (S'First) >= '0' and then S (S'First) <= '9' then
               return S;
            elsif S'Length > 0 and then S (S'First) = ''' then
               return S;
            else
               return "'" & S & "'";
            end if;
         end Escape;

         procedure Configure (Name, Item : in Util.Properties.Value) is
            SQL : constant String := "PRAGMA " & To_String (Name) & "=" & Escape (Item);
         begin
            Log.Info ("Configure database with {0}", SQL);
            Database.Execute (SQL);
         end Configure;

      begin
         Database.Server := Handle;
         Database.Name   := Config.Database;
         DB := Ref.Create (Database.all'Access);

         --  Configure the connection by setting up the SQLite 'pragma X=Y' SQL commands.
         --  Typical configuration includes:
         --    synchronous=OFF
         --    temp_store=MEMORY
         --    encoding='UTF-8'
         Config.Properties.Iterate (Process => Configure'Access);

         Result := Ref.Create (Database.all'Access);
      end;
   end Create_Connection;

   --  ------------------------------
   --  Initialize the SQLite driver.
   --  ------------------------------
   procedure Initialize is
      use type Util.Strings.Name_Access;
   begin
      Log.Debug ("Initializing sqlite driver");

      if Driver.Name = null then
         Driver.Name := Driver_Name'Access;
         Register (Driver'Access);
      end if;
   end Initialize;

end ADO.Drivers.Connections.Sqlite;
