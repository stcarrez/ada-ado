-----------------------------------------------------------------------
--  ADO Sqlite Database -- SQLite Database connections
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2017, 2018, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with System;
with Interfaces.C.Strings;

with Util.Log;
with Util.Log.Loggers;
with Util.Processes.Tools;
with Util.Properties;
with ADO.Sessions;
with ADO.Statements.Sqlite;
with ADO.Statements.Create;
with ADO.Schemas.Sqlite;
with ADO.Sqlite.Runtime;

package body ADO.Connections.Sqlite is

   use ADO.Statements.Sqlite;
   use Interfaces.C;

   pragma Linker_Options (ADO.Sqlite.Runtime.LIBNAME);

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Databases.Sqlite");

   Driver_Name : aliased constant String := "sqlite";
   Driver      : aliased Sqlite_Driver;

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
   overriding
   procedure Begin_Transaction (Database : in out Database_Connection) is
      Transactions : Integer;
   begin
      if Database.Server = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      Util.Concurrent.Counters.Increment (Database.Transactions, Transactions);
      if Transactions = 0 then
         ADO.Statements.Sqlite.Execute (Database.Server, "BEGIN IMMEDIATE TRANSACTION");
      end if;
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   overriding
   procedure Commit (Database : in out Database_Connection) is
      Is_Zero : Boolean;
   begin
      if Database.Server = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      if Util.Concurrent.Counters.Value (Database.Transactions) > 0 then
         Util.Concurrent.Counters.Decrement (Database.Transactions, Is_Zero);
         if Is_Zero then
            ADO.Statements.Sqlite.Execute (Database.Server, "COMMIT TRANSACTION");
         end if;
      end if;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   overriding
   procedure Rollback (Database : in out Database_Connection) is
   begin
      if Database.Server = null then
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;
      if Util.Concurrent.Counters.Value (Database.Transactions) > 0 then
         Util.Concurrent.Counters.Decrement (Database.Transactions);
         ADO.Statements.Sqlite.Execute (Database.Server, "ROLLBACK TRANSACTION");
      end if;
   end Rollback;

   --  ------------------------------
   --  Closes the database connection
   --  ------------------------------
   overriding
   procedure Close (Database : in out Database_Connection) is
      Result : int;
   begin
      Log.Info ("Close connection {0}", Database.Name);
      if Database.Server /= null then
         Result := Sqlite3_H.sqlite3_close_v2 (Database.Server);
         if Result /= Sqlite3_H.SQLITE_OK then
            declare
               Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errstr (Result);
               Msg   : constant String := Strings.Value (Error);
            begin
               Log.Error ("Cannot close database {0}: {1}", To_String (Database.Name), Msg);
            end;
         end if;
         Database.Server := null;
      end if;
   end Close;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      ADO.Schemas.Sqlite.Load_Schema (Database, Schema);
   end Load_Schema;

   --  ------------------------------
   --  Check if the table with the given name exists in the database.
   --  ------------------------------
   overriding
   function Has_Table (Database : in Database_Connection;
                       Name     : in String) return Boolean is
      Stmt  : ADO.Statements.Query_Statement
        := Create.Create_Statement
          (Database.Create_Statement
             ("SELECT COUNT(*) FROM sqlite_master "
              & "WHERE type='table' AND name=:name"));
   begin
      Stmt.Bind_Param ("name", Name);
      Stmt.Execute;

      return Stmt.Get_Result_Integer > 0;
   end Has_Table;

   function Busy_Handler (Arg1  : System.Address;
                          Count : Interfaces.C.int)
                         return Interfaces.C.int with Convention => C;

   function Busy_Handler (Arg1  : System.Address;
                          Count : Interfaces.C.int) return Interfaces.C.int is
   begin
      delay 0.001;
      return (if Count > 10_000 then 0 else 1);
   end Busy_Handler;

   --  ------------------------------
   --  Initialize the database connection manager.
   --  ------------------------------
   overriding
   procedure Create_Connection (D      : in out Sqlite_Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class) is
      URI      : constant String := Config.Get_URI;
      Name     : constant String := Config.Get_Database;
      Database : Database_Connection_Access;
      Filename : Strings.chars_ptr;
      Status   : int;
      Handle   : aliased access Sqlite3;
      Flags    : int
        := Sqlite3_H.SQLITE_OPEN_FULLMUTEX + Sqlite3_H.SQLITE_OPEN_READWRITE;
   begin
      if Config.Is_On (CREATE_NAME) then
         Flags := Flags + Sqlite3_H.SQLITE_OPEN_CREATE;
      end if;
      Filename := Strings.New_String (Name);
      Status := Sqlite3_H.sqlite3_open_v2 (Filename, Handle'Address,
                                           Flags,
                                           Strings.Null_Ptr);

      Strings.Free (Filename);
      if Status /= Sqlite3_H.SQLITE_OK then
         declare
            Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errstr (Status);
            Msg   : constant String := Strings.Value (Error);
         begin
            Log.Error ("Cannot open SQLite database: {0}", Msg);
            raise ADO.Configs.Connection_Error with "Cannot open database: " & Msg;
         end;
      end if;

      Database := new Database_Connection;
      declare

         procedure Configure (Name : in String;
                              Item : in Util.Properties.Value);
         function Escape (Value : in Util.Properties.Value) return String;

         function Escape (Value : in Util.Properties.Value) return String is
            S : constant String := Util.Properties.To_String (Value);
         begin
            if S'Length > 0 and then S (S'First) in '0' .. '9' then
               return S;
            elsif S'Length > 0 and then S (S'First) = ''' then
               return S;
            else
               return "'" & S & "'";
            end if;
         end Escape;

         procedure Configure (Name : in String;
                              Item : in Util.Properties.Value) is
            SQL : constant String := "PRAGMA " & Name & "=" & Escape (Item);
         begin
            if Name /= CREATE_NAME then
               if Util.Strings.Index (Name, '.') = 0 then
                  Log.Info ("Configure database with {0}", SQL);
                  ADO.Statements.Sqlite.Execute (Database.Server, SQL);
               end if;
            end if;

         exception
            when SQL_Error =>
               null;
         end Configure;

      begin
         Database.Server := Handle;
         Database.Name   := To_Unbounded_String (Config.Get_Database);
         Database.URI    := To_Unbounded_String (URI);
         Result := Ref.Create (Database.all'Access);

         Status := Sqlite3_H.sqlite3_busy_handler (Handle,
                                                   Busy_Handler'Access,
                                                   Database.all'Address);
         if Status /= 0 then
            declare
               Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errstr (Status);
               Msg   : constant String := Strings.Value (Error);
            begin
               Log.Error ("Cannot setup SQLite busy handler: {0}", Msg);
            end;
         end if;
         Status := Sqlite3_H.sqlite3_busy_timeout (Handle, 10000);
         if Status /= 0 then
            declare
               Error : constant Strings.chars_ptr := Sqlite3_H.sqlite3_errstr (Status);
               Msg   : constant String := Strings.Value (Error);
            begin
               Log.Error ("Cannot setup SQLite busy timeout: {0}", Msg);
            end;
         end if;

         --  Configure the connection by setting up the SQLite 'pragma X=Y' SQL commands.
         --  Typical configuration includes:
         --    synchronous=OFF
         --    temp_store=MEMORY
         --    encoding='UTF-8'
         Config.Iterate (Process => Configure'Access);
      end;
   end Create_Connection;

   --  ------------------------------
   --  Create the database and initialize it with the schema SQL file.
   --  The `Admin` parameter describes the database connection with
   --  administrator access.  The `Config` parameter describes the target
   --  database connection.
   --  ------------------------------
   overriding
   procedure Create_Database (D           : in out Sqlite_Driver;
                              Admin       : in Configs.Configuration'Class;
                              Config      : in Configs.Configuration'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector) is
      pragma Unreferenced (D, Admin);

      Status        : Integer;
      Database_Path : constant String := Config.Get_Database;
      Command       : constant String :=
        "sqlite3 --batch --init " & Schema_Path & " " & Database_Path;
   begin
      Log.Info ("Creating SQLite database {0}", Database_Path);
      Util.Processes.Tools.Execute (Command, Messages, Status);

      if Status = 0 then
         Log.Info ("Database schema created successfully.");
      elsif Status = 255 then
         Messages.Append ("Command not found: " & Command);
         Log.Error ("Command not found: {0}", Command);
      else
         Messages.Append ("Command " & Command & " failed with exit code "
                            & Util.Strings.Image (Status));
         Log.Error ("Command {0} failed with exit code {1}", Command,
                    Util.Strings.Image (Status));
      end if;
   end Create_Database;

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

   --  ------------------------------
   --  Deletes the SQLite driver.
   --  ------------------------------
   overriding
   procedure Finalize (D : in out Sqlite_Driver) is
   begin
      Log.Debug ("Deleting the sqlite driver");
   end Finalize;

end ADO.Connections.Sqlite;
