-----------------------------------------------------------------------
--  ado-sessions-sources -- Database Sources
--  Copyright (C) 2017 Stephane Carrez
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

--  == Connection string ==
--  The database connection string is an URI that specifies the database driver to use as well
--  as the information for the database driver to connect to the database.
--  The driver connection is a string of the form:
--
--    driver://[host][:port]/[database][?property1][=value1]...
--
--  The database connection string is passed to the session factory that maintains connections
--  to the database (see ADO.Sessions.Factory).
--
package ADO.Sessions.Sources is

   --  ------------------------------
   --  The database connection source
   --  ------------------------------
   --  The <b>DataSource</b> is the factory for getting a connection to the database.
   --  It contains the configuration properties to define which database driver must
   --  be used and which connection parameters the driver has to use to establish
   --  the connection.
   type Data_Source is new ADO.Drivers.Connections.Configuration with private;
   type Data_Source_Access is access all Data_Source'Class;

   --  Attempts to establish a connection with the data source
   --  that this Data_Source object represents.
--   procedure Create_Connection (Controller : in Data_Source)
--                         return ADO.Master_Connection'Class;


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
   type Replicated_DataSource is new Data_Source with private;
   type Replicated_DataSource_Access is access all Replicated_DataSource'Class;

   --  Set the master data source
   procedure Set_Master (Controller : in out Replicated_DataSource;
                         Master     : in Data_Source_Access);

   --  Get the master data source
   function Get_Master (Controller : in Replicated_DataSource)
                       return Data_Source_Access;

   --  Set the slave data source
   procedure Set_Slave (Controller : in out Replicated_DataSource;
                        Slave      : in Data_Source_Access);

   --  Get the slace data source
   function Get_Slave (Controller : in Replicated_DataSource)
                      return Data_Source_Access;

   --  Get a slave database connection
--   function Get_Slave_Connection (Controller : in Replicated_DataSource)
--                                 return Connection'Class;

private

   type Data_Source is new ADO.Drivers.Connections.Configuration with null record;

   type Replicated_DataSource is new Data_Source with record
      Master : Data_Source_Access := null;
      Slave  : Data_Source_Access := null;
   end record;

end ADO.Sessions.Sources;
