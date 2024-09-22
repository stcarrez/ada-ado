-----------------------------------------------------------------------
--  ado-sqlite -- SQLite Database Drivers
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Configs;
with ADO.Connections.Sqlite;

package body ADO.Sqlite is

   --  ------------------------------
   --  Initialize the SQLite driver.
   --  ------------------------------
   procedure Initialize is
   begin
      ADO.Connections.Sqlite.Initialize;
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   --  ------------------------------
   procedure Initialize (Config : in String) is
   begin
      ADO.Configs.Initialize (Config);
      Initialize;
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library and configure the runtime with the given properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
   begin
      ADO.Configs.Initialize (Config);
      Initialize;
   end Initialize;

end ADO.Sqlite;
