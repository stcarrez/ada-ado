-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
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

with Ada.Strings.Unbounded;
with Ada.Finalization;

with ADO.Statements;
with ADO.Schemas;
with Util.Properties;

--  The <b>ADO.Drivers</b> package represents the database driver that will create
--  database connections and provide the database specific implementation.
package ADO.Drivers is

   use ADO.Statements;

   use Ada.Strings.Unbounded;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception;

   --  Raised for all errors reported by the database
   DB_Error : exception;

   --  ------------------------------
   --  Database connection implementation
   --  ------------------------------
   --
   type Database_Connection is abstract tagged limited record
      Count : Natural := 0;
   end record;
   type Database_Connection_Access is access all Database_Connection'Class;

   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement'Class is abstract;

   --  Start a transaction.
   procedure Begin_Transaction (Database : in out Database_Connection) is abstract;

   --  Commit the current transaction.
   procedure Commit (Database : in out Database_Connection) is abstract;

   --  Rollback the current transaction.
   procedure Rollback (Database : in out Database_Connection) is abstract;

   procedure Execute (Database : in out Database_Connection;
                      SQL : in Query_String) is abstract;

   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String;
                      Id       : out Identifier) is abstract;

   --  Load the database schema definition for the current database.
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is abstract;

   --  Closes the database connection
   procedure Close (Database : in out Database_Connection) is abstract;

   --  Releases the connection and all resources used to maintain it.
   procedure Release (Database : access Database_Connection) is abstract;

   --  ------------------------------
   --  The database configuration properties
   --  ------------------------------
   type Configuration is new Ada.Finalization.Controlled with private;
   type Configuration_Access is access all Configuration'Class;

   --  Set the connection URL to connect to the database.
   --  The driver connection is a string of the form:
   --
   --   driver://[host][:port]/[database][?property1][=value1]...
   --
   --  If the string is invalid or if the driver cannot be found,
   --  the Connection_Error exception is raised.
   procedure Set_Connection (Controller : in out Configuration;
                             URI        : in String);

   --  Set a property on the datasource for the driver.
   --  The driver can retrieve the property to configure and open
   --  the database connection.
   procedure Set_Property (Controller : in out Configuration;
                           Name       : in String;
                           Value      : in String);

   --  Get a property from the data source configuration.
   --  If the property does not exist, an empty string is returned.
   function Get_Property (Controller : in Configuration;
                          Name       : in String) return String;

   --  Create a new connection using the configuration parameters.
   procedure Create_Connection (Config : in Configuration'Class;
                                Result : out Database_Connection_Access);

   --  ------------------------------
   --  Database Driver
   --  ------------------------------
   type Driver is abstract tagged limited private;
   type Driver_Access is access all Driver'Class;

   --  Create a new connection using the configuration parameters.
   procedure Create_Connection (D      : in out Driver;
                                Config : in Configuration'Class;
                                Result : out Database_Connection_Access) is abstract;

   --  Register a database driver.
   procedure Register (Driver : in Driver_Access);

   --  Get a database driver given its name.
   function Get_Driver (Name : in String) return Driver_Access;

private

   type Driver is abstract new Ada.Finalization.Limited_Controlled with record
      Name : Unbounded_String;
   end record;

   type Configuration is new Ada.Finalization.Controlled with record
      URI        : Unbounded_String := Null_Unbounded_String;
      Server     : Unbounded_String := Null_Unbounded_String;
      Port       : Integer := 0;
      Database   : Unbounded_String := Null_Unbounded_String;
      Properties : Util.Properties.Manager;
      Driver     : Driver_Access;
   end record;

end ADO.Drivers;
