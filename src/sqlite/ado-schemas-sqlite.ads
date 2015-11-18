-----------------------------------------------------------------------
--  ado-schemas-sqlite -- SQLite Database Schemas
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with ADO.Drivers.Connections;
package ADO.Schemas.Sqlite is

   --  Load the database schema
   procedure Load_Schema (C      : in ADO.Drivers.Connections.Database_Connection'Class;
                          Schema : out Schema_Definition);

end ADO.Schemas.Sqlite;
