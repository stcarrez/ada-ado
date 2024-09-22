-----------------------------------------------------------------------
--  ado-sqlite -- SQLite Database Drivers
--  Copyright (C) 2019, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Properties;

--  === SQLite Database Driver ===
--  The SQLite database driver can be initialize explicitly by using the `ado_sqlite`
--  GNAT project and calling the initialization procedure.
--
--    ADO.Sqlite.Initialize ("db.properties");
--
--  The set of configuration properties can be set programatically and passed to the
--  `Initialize` operation.
--
--    Config : Util.Properties.Manager;
--    ...
--      Config.Set ("ado.database", "sqlite:///regtests.db?synchronous=OFF&encoding=UTF-8");
--      Config.Set ("ado.queries.path", ".;db");
--      ADO.Sqlite.Initialize (Config);
--
--  The SQLite database driver will pass all the properties as SQLite `pragma` allowing
--  the configuration of the SQLite database.
--
package ADO.Sqlite is

   --  Initialize the SQLite driver.
   procedure Initialize;

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

end ADO.Sqlite;
