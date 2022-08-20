-----------------------------------------------------------------------
--  ado-connections-sqlite -- SQLite Database connections
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2019, 2021, 2022 Stephane Carrez
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
private with Ada.Strings.Unbounded;
private with Util.Concurrent.Counters;
private with Ada.Containers.Doubly_Linked_Lists;
package ADO.Connections.Sqlite is

   --  Create database option.
   CREATE_NAME : constant String := "create";

   subtype Sqlite3 is Sqlite3_H.sqlite3;

   --  The database connection manager
   type Sqlite_Driver is limited private;

   --  Initialize the SQLite driver.
   procedure Initialize;

private

   use Ada.Strings.Unbounded;

   --  Database connection implementation
   type Database_Connection is new ADO.Connections.Database_Connection with record
      Server       : aliased access Sqlite3_H.sqlite3;
      Name         : Unbounded_String;
      URI          : Unbounded_String;
      Transactions : Util.Concurrent.Counters.Counter_Access;
   end record;
   type Database_Connection_Access is access all Database_Connection'Class;

   --  Get the database driver which manages this connection.
   overriding
   function Get_Driver (Database : in Database_Connection)
                        return Driver_Access;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement_Access;

   --  Create a delete statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access;

   --  Create an insert statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access;

   --  Create an update statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access;

   --  Start a transaction.
   overriding
   procedure Begin_Transaction (Database : in out Database_Connection);

   --  Commit the current transaction.
   overriding
   procedure Commit (Database : in out Database_Connection);

   --  Rollback the current transaction.
   overriding
   procedure Rollback (Database : in out Database_Connection);

   --  Load the database schema definition for the current database.
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition);

   --  Check if the table with the given name exists in the database.
   overriding
   function Has_Table (Database : in Database_Connection;
                       Name     : in String) return Boolean;

   --  Closes the database connection
   overriding
   procedure Close (Database : in out Database_Connection);

   type SQLite_Database is record
      Server       : aliased access Sqlite3_H.sqlite3;
      Transactions : Util.Concurrent.Counters.Counter_Access;
      Name         : Unbounded_String;
      URI          : Unbounded_String;
   end record;

   package Database_List is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => SQLite_Database);

   protected type Sqlite_Connections is

      procedure Open (Config : in Configuration'Class;
                      Result : in out Ref.Ref'Class);

      procedure Clear;

   private
      List : Database_List.List;
   end Sqlite_Connections;

   type Sqlite_Driver is new ADO.Connections.Driver with record
      Map : Sqlite_Connections;
   end record;

   --  Create a new SQLite connection using the configuration parameters.
   overriding
   procedure Create_Connection (D      : in out Sqlite_Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class);

   --  Create the database and initialize it with the schema SQL file.
   --  The `Admin` parameter describes the database connection with administrator access.
   --  The `Config` parameter describes the target database connection.
   overriding
   procedure Create_Database (D           : in out Sqlite_Driver;
                              Admin       : in Configs.Configuration'Class;
                              Config      : in Configs.Configuration'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector);

   --  Deletes the SQLite driver.
   overriding
   procedure Finalize (D : in out Sqlite_Driver);

end ADO.Connections.Sqlite;
