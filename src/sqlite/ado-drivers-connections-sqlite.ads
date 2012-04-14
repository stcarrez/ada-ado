-----------------------------------------------------------------------
--  ADO Sqlite Database -- SQLite Database connections
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with System;
with Interfaces.C;
package ADO.Drivers.Connections.Sqlite is

   subtype Sqlite_Access is System.Address;

   --  The database connection manager
   type Sqlite_Driver is limited private;

   --  Initialize the SQLite driver.
   procedure Initialize;

   --  Check for an error after executing a sqlite statement.
   procedure Check_Error (Connection : in Sqlite_Access;
                          Result     : in Interfaces.C.int);

private

   --  Database connection implementation
   type Database_Connection is new ADO.Drivers.Connections.Database_Connection with record
      Server : aliased Sqlite_Access;
      Name   : Unbounded_String;
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

   --  Closes the database connection
   overriding
   procedure Close (Database : in out Database_Connection);

   --  Releases the sqlite connection if it is open
   overriding
   procedure Finalize (Database : in out Database_Connection);

   procedure Execute (Database : in out Database_Connection;
                      SQL : in Query_String);

   type Sqlite_Driver is new ADO.Drivers.Connections.Driver with null record;

   --  Create a new SQLite connection using the configuration parameters.
   overriding
   procedure Create_Connection (D      : in out Sqlite_Driver;
                                Config : in Configuration'Class;
                                Result : out ADO.Drivers.Connections.Database_Connection_Access);

end ADO.Drivers.Connections.Sqlite;
