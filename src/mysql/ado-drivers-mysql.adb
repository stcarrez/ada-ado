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

with Interfaces.C.Strings;
with Util.Log;
with Util.Log.Loggers;
with ADO.Statements.Mysql;
with ADO.C;
package body ADO.Drivers.Mysql is

   use ADO.Statements.Mysql;
   use Util.Log;
   use Interfaces.C;

   pragma Linker_Options ("-lmysqlclient");

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Databases.Mysql");

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
      if Database.Autocommit then
         Database.Execute ("set autocommit=0");
         Database.Autocommit := False;
      end if;
      Database.Execute ("start transaction;");
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   procedure Commit (Database : in out Database_Connection) is
      Result : char;
   begin
      Result := mysql_commit (Database.Server);
      if Result /= nul then
         raise DB_Error with "Cannot commit transaction";
      end if;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   procedure Rollback (Database : in out Database_Connection) is
      Result : char;
   begin
      Result := mysql_rollback (Database.Server);
      if Result /= nul then
         raise DB_Error with "Cannot rollback transaction";
      end if;
   end Rollback;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      null;
   end Load_Schema;

   --  ------------------------------
   --  Execute a simple SQL statement
   --  ------------------------------
   overriding
   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String) is
      SQL_Stat  : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (SQL);
      Result    : int;
   begin
      Log.Debug ("Execute SQL: {0}", SQL);
      if Database.Server = null then
         Log.Error ("Database connection is not open");
      end if;

      Result := Mysql_Query (Database.Server, ADO.C.To_C (SQL_Stat));
   end Execute;

   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String;
                      Id       : out Identifier) is
   begin
      --  Execute (Database, SQL);
      null;
      --  Id := Identifier (sqlite3_h.sqlite3_last_insert_rowid (Database.H));
   end Execute;

   --  ------------------------------
   --  Closes the database connection
   --  ------------------------------
   overriding
   procedure Close (Database : in out Database_Connection) is
   begin
      Log.Info ("Closing connection {0}", Database.Name);
      if Database.Server /= null then
         mysql_close (Database.Server);
         Database.Server := null;
      end if;
   end Close;

   --  ------------------------------
   --  Releases the mysql connection if it is open
   --  ------------------------------
   procedure Finalize (Database : in out Database_Connection) is
   begin
      Log.Debug ("Release connection {0}", Database.Name);
      Database.Close;
   end Finalize;

   --  Initialize the database connection manager.
   --
   --  mysql://localhost:3306/db
   --
   procedure Create_Connection (D      : in out Mysql_Driver;
                                Config : in Configuration'Class;
                                Result : out ADO.Drivers.Database_Connection_Access) is

      pragma Unreferenced (D);

      Server   : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Config.Server);
      Name     : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Config.Database);
      Login    : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Config.Get_Property ("user"));
      Password : constant ADO.C.String_Ptr := C.To_String_Ptr (Config.Get_Property ("password"));
      Socket   : ADO.C.String_Ptr;
      Port     : unsigned := unsigned (Config.Port);
      Flags    : constant unsigned_long := 0;
      Connection : Mysql_Access;

      Socket_Path : constant String := Config.Get_Property ("socket");
      Database : constant Database_Connection_Access := new Database_Connection;
   begin
      if Socket_Path /= "" then
         ADO.C.Set_String (Socket, Socket_Path);
      end if;
      if Port = 0 then
         Port := 3306;
      end if;

      Log.Info ("Connecting to {0}:{1}", To_String (Config.Server), To_String (Config.Database));
      Log.Debug ("user={0} password={1}", Config.Get_Property ("user"),
                 Config.Get_Property ("password"));
      Connection := mysql_init (null);
      Database.Server := mysql_real_connect (Connection, ADO.C.To_C (Server),
                                             ADO.C.To_C (Login), ADO.C.To_C (Password),
                                             ADO.C.To_C (Name), Port, ADO.C.To_C (Socket), Flags);

      if Database.Server = null then
         declare
            Message : constant String := Strings.Value (Mysql_Error (Connection));
         begin
            Log.Error ("Cannot connect to '{0}': {1}", To_String (Config.URI), Message);
            mysql_close (Connection);
            raise Connection_Error with "Cannot connect to mysql server: " & Message;
         end;
      end if;

      Database.Name  := Config.Database;
      Database.Count := 1;
      Result         := Database.all'Access;
   end Create_Connection;

   Driver : aliased Mysql_Driver;

   procedure Initialize is
   begin
      Log.Debug ("Initializing mysql driver");

      Driver.Name := To_Unbounded_String ("mysql");
      Register (Driver'Access);
   end Initialize;

   overriding
   procedure Finalize (D : in out Mysql_Driver) is
      pragma Unreferenced (D);
   begin
      Log.Debug ("Deleting the mysql driver");

      mysql_server_end;
   end Finalize;

end ADO.Drivers.Mysql;
