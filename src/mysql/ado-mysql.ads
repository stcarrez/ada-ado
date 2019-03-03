-----------------------------------------------------------------------
--  ado-mysql -- MySQL Database Drivers
--  Copyright (C) 2019 Stephane Carrez
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

--  === MySQL Database Driver ===
--  The MySQL database driver can be initialize explicitly by using the `ado_mysql`
--  GNAT project and calling the initialization procedure.
--
--    ADO.Mysql.Initialize ("db.properties");
--
--  The set of configuration properties can be set programatically and passed to the
--  `Initialize` operation.
--
--    Config : Util.Properties.Manager;
--    ...
--      Config.Set ("ado.database", "mysql://localhost:3306/ado_test");
--      Config.Set ("ado.queries.path", ".;db");
--      ADO.Mysql.Initialize (Config);
--
--  The MySQL database driver supports the following properties:
--
--  | Name        | Description       |
--  | ----------- | ---------      |
--  | user        | The user name to connect to the server |
--  | password    | The user password to connect to the server |
--  | socket      | The optional Unix socket path for a Unix socket base connection |
--  | encoding    | The encoding to be used for the connection (ex: UTF-8) |
--
package ADO.Mysql is

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

end ADO.Mysql;
