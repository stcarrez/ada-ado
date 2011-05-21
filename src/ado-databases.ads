-----------------------------------------------------------------------
--  ADO Databases -- Database Connections
--  Copyright (C) 2010 Stephane Carrez
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

with Ada.Finalization;

with ADO.Statements;
with ADO.Schemas;
with ADO.Drivers;

package ADO.Databases is

   use ADO.Statements;

   --  Raised for all errors reported by the database
   DB_Error : exception;

   --  Raised if the database connection is not open.
   NOT_OPEN : exception;

   NOT_FOUND : exception;

   NO_DATABASE : exception;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception;

   --  The database connection status
   type Connection_Status is (OPEN, CLOSED);

   --  ---------
   --  Database connection
   --  ---------
   --  Read-only database connection (or slave connection).
   --
   type Connection is tagged private;

   --  Get the database connection status.
   function Get_Status (Database : in Connection) return Connection_Status;

   --  Close the database connection
   procedure Close (Database : in out Connection);

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement;

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Connection;
                              Query    : in String)
                              return Query_Statement;

   --  Load the database schema definition for the current database.
   procedure Load_Schema (Database : in Connection;
                          Schema   : out ADO.Schemas.Schema_Definition);

   --  ---------
   --  Master Database connection
   --  ---------
   --  Read-write database connection.
   type Master_Connection is new Connection with private;

   --  Start a transaction.
   procedure Begin_Transaction (Database : in out Master_Connection);

   --  Commit the current transaction.
   procedure Commit (Database : in out Master_Connection);

   --  Rollback the current transaction.
   procedure Rollback (Database : in out Master_Connection);

   --  Execute an SQL statement
--     procedure Execute (Database : in Master_Connection;
--                        SQL      : in Query_String);

   --  Execute an SQL statement
--     procedure Execute (Database : in Master_Connection;
--                        SQL      : in Query_String;
--                        Id       : out Identifier);

   --  Create a delete statement.
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement;

   --  Create an insert statement.
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement;

   --  Create an update statement.
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement;

   --  ------------------------------
   --  The database connection source
   --  ------------------------------
   --  The <b>DataSource</b> is the factory for getting a connection to the database.
   --  It contains the configuration properties to define which database driver must
   --  be used and which connection parameters the driver has to use to establish
   --  the connection.
   type DataSource is new ADO.Drivers.Configuration with private;
   type DataSource_Access is access all DataSource'Class;

   --  Attempts to establish a connection with the data source
   --  that this DataSource object represents.
   function Get_Connection (Controller : in DataSource)
                         return Master_Connection'Class;


   --  ------------------------------
   --  Replicated Data Source
   --  ------------------------------
   --  The replicated data source supports a Master/Slave database configuration.
   --  When using this data source, the master is used to execute
   --  update, insert, delete and also query statements.  The slave is used
   --  to execute query statements.  The master and slave are represented by
   --  two separate data sources.  This allows to have a master on one server,
   --  use a specific user/password and get a slave on another server with other
   --  credentials.
   type Replicated_DataSource is new DataSource with private;
   type Replicated_DataSource_Access is access all Replicated_DataSource'Class;

   --  Set the master data source
   procedure Set_Master (Controller : in out Replicated_DataSource;
                         Master     : in DataSource_Access);

   --  Get the master data source
   function Get_Master (Controller : in Replicated_DataSource)
                       return DataSource_Access;

   --  Set the slave data source
   procedure Set_Slave (Controller : in out Replicated_DataSource;
                        Slave      : in DataSource_Access);

   --  Get the slace data source
   function Get_Slave (Controller : in Replicated_DataSource)
                      return DataSource_Access;

   --  Get a slave database connection
   function Get_Slave_Connection (Controller : in Replicated_DataSource)
                                 return Connection'Class;

private

   type Connection is new Ada.Finalization.Controlled with record
      Impl : ADO.Drivers.Database_Connection_Access := null;
   end record;

   --  Adjust the connection reference counter
   overriding
   procedure Adjust (Object : in out Connection);

   --  Releases the connection reference counter
   overriding
   procedure Finalize (Object : in out Connection);

   type Master_Connection is new Connection with null record;

   type DataSource is new ADO.Drivers.Configuration with null record;

   type Replicated_DataSource is new DataSource with record
      Master : DataSource_Access := null;
      Slave  : DataSource_Access := null;
   end record;

end ADO.Databases;
