-----------------------------------------------------------------------
--  ado-mysql -- Database Drivers
--  Copyright (C) 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Configs;
with ADO.Connections.Mysql;

package body ADO.Mysql is

   --  ------------------------------
   --  Initialize the Mysql driver.
   --  ------------------------------
   procedure Initialize is
   begin
      ADO.Connections.Mysql.Initialize;
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

end ADO.Mysql;
