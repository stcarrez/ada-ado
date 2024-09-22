-----------------------------------------------------------------------
--  ado-postgresql -- PostgreSQL Database Drivers
--  Copyright (C) 2019, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Properties;

--  === PostgreSQL Database Driver ===
--  The PostgreSQL database driver can be initialize explicitly by using the `ado_postgresql`
--  GNAT project and calling the initialization procedure.
--
--    ADO.Postgresql.Initialize ("db.properties");
--
--  The set of configuration properties can be set programatically and passed to the
--  `Initialize` operation.
--
--    Config : Util.Properties.Manager;
--    ...
--      Config.Set ("ado.database", "postgresql://localhost:5432/ado_test?user=ado&password=ado");
--      Config.Set ("ado.queries.path", ".;db");
--      ADO.Postgresql.Initialize (Config);
--
--  The PostgreSQL database driver supports the following properties:
--
--  | Name        | Description       |
--  | ----------- | ---------      |
--  | user        | The user name to connect to the server |
--  | password    | The user password to connect to the server |
--
package ADO.Postgresql is

   --  Initialize the Postgresql driver.
   procedure Initialize;

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

end ADO.Postgresql;
