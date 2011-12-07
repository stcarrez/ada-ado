-----------------------------------------------------------------------
--  ADO Sqlite Database -- SQLite Database connections
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

with Sqlite3_H;
with Interfaces.C.Strings;

with Util.Log;
with Util.Log.Loggers;
with ADO.Statements.Sqlite;

package body ADO.Drivers.Sqlite is

   use Util.Log;
   use ADO.Statements.Sqlite;
   use Interfaces.C;

   pragma Linker_Options ("-lsqlite3");

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Databases.Sqlite");

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

      SQL_Stat  : Strings.chars_ptr := Strings.New_String (SQL);
      Result    : int;
      Error_Msg : Strings.chars_ptr;
   begin
      Log.Debug ("Execute: {0}", SQL);

      for Retry in 1 .. 100 loop
         Result := Sqlite3_H.sqlite3_exec (Database.Server, SQL_Stat, No_Callback,
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
      Strings.Free (SQL_Stat);
   end Execute;

   --  ------------------------------
   --  Closes the database connection
   --  ------------------------------
   overriding
   procedure Close (Database : in out Database_Connection) is
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

      Database.Count := Database.Count - 1;
--        if Database.Count <= 1 then
--           Result := Sqlite3_H.Sqlite3_Close (Database.Server);
--        end if;
--        Database.Server := System.Null_Address;
      pragma Unreferenced (Result);
   end Finalize;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      null;
   end Load_Schema;

   DB : ADO.Drivers.Database_Connection_Access := null;

   --  ------------------------------
   --  Initialize the database connection manager.
   --  ------------------------------
   procedure Create_Connection (D      : in out Sqlite_Driver;
                                Config : in Configuration'Class;
                                Result : out ADO.Drivers.Database_Connection_Access) is
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

      if DB /= null then
         Result := DB;
         DB.Count := DB.Count + 1;
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

         procedure Configure (Name, Item : in Util.Properties.Value) is
            SQL : constant String := "PRAGMA " & To_String (Name) & "=" & To_String (Item);
         begin
            Database.Execute (SQL);
         end Configure;

      begin
         Database.Server := Handle;
         Database.Count  := 2;
         Database.Name   := Config.Database;

         --  Configure the connection by setting up the SQLite 'pragma X=Y' SQL commands.
         --  Typical configuration includes:
         --    synchronous=OFF
         --    temp_store=MEMORY
         --    encoding='UTF-8'
         Config.Properties.Iterate (Process => Configure'Access);

         Result := Database.all'Access;
         DB := Result;
      end;
   end Create_Connection;

   --  ------------------------------
   --  Initialize the SQLite driver.
   --  ------------------------------
   procedure Initialize is
   begin
      Driver.Name := Driver_Name'Access;
      Register (Driver'Access);
   end Initialize;

end ADO.Drivers.Sqlite;
