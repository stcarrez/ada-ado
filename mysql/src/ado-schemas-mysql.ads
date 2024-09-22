-----------------------------------------------------------------------
--  ado.schemas.mysql -- Mysql Database Schemas
--  Copyright (C) 2009, 2010, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Connections;
package ADO.Schemas.Mysql is

   --  Load the database schema
   procedure Load_Schema (C      : in ADO.Connections.Database_Connection'Class;
                          Schema : out Schema_Definition);

end ADO.Schemas.Mysql;
