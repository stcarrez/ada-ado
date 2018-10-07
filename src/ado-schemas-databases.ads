-----------------------------------------------------------------------
--  ado-schemas-databases -- Database creation
--  Copyright (C) 2018 Stephane Carrez
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

with Util.Strings.Vectors;
with ADO.Sessions.Sources;
package ADO.Schemas.Databases is

   --  Create the database and initialize it with the schema SQL file.
   --  The `Admin` parameter describes the database connection with administrator access.
   --  The `Config` parameter describes the target database connection: this is the
   --  database that must be created and configured.  The `Schema_Path` is the path
   --  of the SQL file that can be used to populate the database with the schema.
   --  The `Messages` vector will contain the messages produced during the setup and
   --  configuration of the database.
   --
   --  For the `sqlite` driver, the `Admin` parameter is not used.
   procedure Create_Database (Admin       : in ADO.Sessions.Sources.Data_Source'Class;
                              Config      : in ADO.Sessions.Sources.Data_Source'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector);

end ADO.Schemas.Databases;
