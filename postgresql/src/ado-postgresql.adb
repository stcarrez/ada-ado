-----------------------------------------------------------------------
--  ado-postgresql -- PostgreSQL Database Drivers
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Configs;
with ADO.Connections.Postgresql;

package body ADO.Postgresql is
   --  ------------------------------
   --  Initialize the Postgresql driver.
   --  ------------------------------
   procedure Initialize is
   begin
      ADO.Connections.Postgresql.Initialize;
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   --  ------------------------------
   procedure Initialize (Config : in String) is
   begin
      ADO.Configs.Initialize (Config);
      ADO.Connections.Postgresql.Initialize;
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library and configure the runtime with the given properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
   begin
      ADO.Configs.Initialize (Config);
      ADO.Connections.Postgresql.Initialize;
   end Initialize;

end ADO.Postgresql;
