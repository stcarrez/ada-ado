-----------------------------------------------------------------------
--  ado-schemas-sqlite -- SQLite Database Schemas
--  Copyright (C) 2015, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Connections;
package ADO.Schemas.Sqlite is

   --  Load the database schema
   procedure Load_Schema (C      : in ADO.Connections.Database_Connection'Class;
                          Schema : out Schema_Definition);

end ADO.Schemas.Sqlite;
