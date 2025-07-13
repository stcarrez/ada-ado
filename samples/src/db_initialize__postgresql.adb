-----------------------------------------------------------------------
--  db_initialize - Database driver setup (multi-driver)
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Configs;
with ADO.Drivers;

with Ada.Directories;

with Util.Log.Loggers;
with Util.Properties;

procedure DB_Initialize (Props   : in out Util.Properties.Manager) is
begin
   --  Initialize the database drivers.
   if Ada.Directories.Exists ("samples.properties") then
      Props.Load_Properties ("samples.properties");
   end if;
   Util.Log.Loggers.Initialize (Props, "example.");

   ADO.Drivers.Initialize (Props);
end DB_Initialize;
