-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2013, 2018 Stephane Carrez
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

with Util.Properties;

--  == Database Drivers ==
--  Database drivers provide operations to access the database.  These operations are
--  specific to the database type and the `ADO.Drivers` package among others provide
--  an abstraction that allows to make the different databases look like they have almost
--  the same interface.
--
--  A database driver exists for SQLite, MySQL and PostgreSQL. The driver
--  is either statically linked to the application or it can be loaded dynamically if it was
--  built as a shared library.  For a dynamic load, the driver shared library name must be
--  prefixed by `libada_ado_`.  For example, for a `mysql` driver, the shared
--  library name is `libada_ado_mysql.so`.
--
--  | Driver name | Database       |
--  | ----------- | ---------      |
--  | mysql       | MySQL, MariaDB |
--  | sqlite      | SQLite         |
--  | postgresql  | PostgreSQL     |
--
--  The database drivers are initialized automatically but in some cases, you may want
--  to control some database driver configuration parameter.  In that case,
--  the initialization must be done only once before creating a session
--  factory and getting a database connection.  The initialization can be made using
--  a property file which contains the configuration for the database drivers and
--  the database connection properties.  For such initialization, you will have to
--  call one of the `Initialize` operation from the `ADO.Drivers` package.
--
--    ADO.Drivers.Initialize ("db.properties");
--
--  The set of configuration properties can be set programatically and passed to the
--  `Initialize` operation.
--
--    Config : Util.Properties.Manager;
--    ...
--      Config.Set ("ado.database", "sqlite:///mydatabase.db");
--      Config.Set ("ado.queries.path", ".;db");
--      ADO.Drivers.Initialize (Config);
--
--  Once initialized, a configuration property can be retrieved by using the `Get_Config`
--  operation.
--
--    URI : constant String := ADO.Drivers.Get_Config ("ado.database");
--
--  Dynamic loading of database drivers is disabled by default for security reasons and
--  it can be enabled by setting the following property in the configuration file:
--
--    ado.drivers.load=true
--
--  Dynamic loading is triggered when a database connection string refers to a database
--  driver which is not known.
--
--  @include ado-mysql.ads
--  @include ado-sqlite.ads
--  @include ado-postgresql.ads
package ADO.Drivers is

   --  Raised for all errors reported by the database.
   Database_Error   : exception;

   type Driver_Index is new Natural range 0 .. 4;

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

   --  Get the global configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Name    : in String;
                        Default : in String := "") return String;

   --  Returns true if the global configuration property is set to true/on.
   function Is_On (Name   : in String) return Boolean;

   --  Initialize the drivers which are available.
   procedure Initialize;

end ADO.Drivers;
