-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2017, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Configs;

package body ADO.Drivers is

   --  ------------------------------
   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   --  ------------------------------
   procedure Initialize (Config : in String) is
   begin
      ADO.Configs.Initialize (Config);
      ADO.Drivers.Initialize;
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library and configure the runtime with the given properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
   begin
      ADO.Configs.Initialize (Config);

      --  Initialize the drivers.
      ADO.Drivers.Initialize;
   end Initialize;

   --  Initialize the drivers which are available.
   procedure Initialize is separate;

end ADO.Drivers;
