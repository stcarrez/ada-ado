-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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

--  == Introduction ==
--  The <b>ADO.Drivers</b> package represents the database driver that will create
--  database connections and provide the database specific implementation.  The driver
--  is either statically linked to the application and can be loaded dynamically if it was
--  built as a shared library.  For a dynamic load, the driver shared library name must be
--  prefixed by <b>libada_ado_</b>.  For example, for a <tt>mysql</tt> driver, the shared
--  library name is <tt>libada_ado_mysql.so</tt>.
--
--  === Initialization ===
--  The <b>ADO</b> runtime must be initialized by calling one of the <b>Initialize</b> operation.
--  A property file contains the configuration for the database drivers and the database
--  connection properties.
--
--    ADO.Drivers.Initialize ("db.properties");
--
--  Once initialized, a configuration property can be retrieved by using the <tt>Get_Config</tt>
--  operation.
--
--    URI : constant String := ADO.Drivers.Get_Config ("ado.database");
--
--  === Connection string ===
--  The database connection string is an URI that specifies the database driver to use as well
--  as the information for the database driver to connect to the database.
--  The driver connection is a string of the form:
--
--    driver://[host][:port]/[database][?property1][=value1]...
--
--  The database connection string is passed to the session factory that maintains connections
--  to the database (see ADO.Sessions.Factory).
--
package ADO.Drivers is

   use Ada.Strings.Unbounded;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception;

   --  Raised for all errors reported by the database
   DB_Error : exception;

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

end ADO.Drivers;
