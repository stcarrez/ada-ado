-----------------------------------------------------------------------
--  ado-mysql -- MySQL Database Drivers
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

   --  Initialize the Mysql driver.
   procedure Initialize;

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

end ADO.Mysql;
