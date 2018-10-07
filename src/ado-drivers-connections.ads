-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2016, 2017, 2018 Stephane Carrez
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
with ADO.Configs;
with Util.Strings;
with Util.Strings.Vectors;
with Util.Refs;

--  The <b>ADO.Drivers</b> package represents the database driver that will create
--  database connections and provide the database specific implementation.
package ADO.Drivers.Connections is

   use ADO.Statements;

   type Driver is abstract tagged limited private;
   type Driver_Access is access all Driver'Class;

   --  ------------------------------
   --  Database connection implementation
   --  ------------------------------
   --
   type Database_Connection is abstract new Util.Refs.Ref_Entity with record
      Ident : String (1 .. 8) := (others => ' ');
   end record;
   type Database_Connection_Access is access all Database_Connection'Class;

   --  Get the database driver index.
   function Get_Driver_Index (Database : in Database_Connection) return Driver_Index;

   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is abstract;

   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement_Access is abstract;

   --  Create a delete statement.
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is abstract;

   --  Create an insert statement.
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is abstract;

   --  Create an update statement.
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is abstract;

   --  Start a transaction.
   procedure Begin_Transaction (Database : in out Database_Connection) is abstract;

   --  Commit the current transaction.
   procedure Commit (Database : in out Database_Connection) is abstract;

   --  Rollback the current transaction.
   procedure Rollback (Database : in out Database_Connection) is abstract;

   --  Load the database schema definition for the current database.
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is abstract;

   --  Create the database and initialize it with the schema SQL file.
   procedure Create_Database (Database    : in Database_Connection;
                              Config      : in Configs.Configuration'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector) is abstract;

   --  Get the database driver which manages this connection.
   function Get_Driver (Database : in Database_Connection)
                        return Driver_Access is abstract;

   --  Closes the database connection
   procedure Close (Database : in out Database_Connection) is abstract;

   package Ref is
      new Util.Refs.Indefinite_References (Element_Type   => Database_Connection'Class,
                                           Element_Access => Database_Connection_Access);

   --  ------------------------------
   --  The database configuration properties
   --  ------------------------------
   type Configuration is new ADO.Configs.Configuration with null record;

   --  Get the driver index that corresponds to the driver for this database connection string.
   function Get_Driver (Config : in Configuration) return Driver_Index;

   --  Create a new connection using the configuration parameters.
   procedure Create_Connection (Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class);

   --  ------------------------------
   --  Database Driver
   --  ------------------------------

   --  Create a new connection using the configuration parameters.
   procedure Create_Connection (D      : in out Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class) is abstract;

   --  Get the driver unique index.
   function Get_Driver_Index (D : in Driver) return Driver_Index;

   --  Get the driver name.
   function Get_Driver_Name (D : in Driver) return String;

   --  Register a database driver.
   procedure Register (Driver : in Driver_Access);

   --  Get a database driver given its name.
   function Get_Driver (Name : in String) return Driver_Access;

private

   type Driver is abstract new Ada.Finalization.Limited_Controlled with record
      Name  : Util.Strings.Name_Access;
      Index : Driver_Index;
   end record;

end ADO.Drivers.Connections;
