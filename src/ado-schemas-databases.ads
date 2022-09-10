-----------------------------------------------------------------------
--  ado-schemas-databases -- Database creation and upgrade
--  Copyright (C) 2018, 2022 Stephane Carrez
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
with Ada.Containers.Doubly_Linked_Lists;
with Util.Strings.Vectors;
with ADO.Sessions.Sources;
package ADO.Schemas.Databases is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   type Upgrade_Type is record
      Version : Positive;
      Name    : UString;
      Depend  : UString;
      Path    : UString;
   end record;

   package Upgrade_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Upgrade_Type);

   subtype Upgrade_List is Upgrade_Lists.List;

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

   --  Scan the directory for migration script and check with the database to define
   --  the database upgrade scripts that must be executed.  The result list must then
   --  be sorted by using `Sort_Migration` to honor the module dependencies.
   procedure Scan_Migration (Session : in ADO.Sessions.Session'Class;
                             Path    : in String;
                             Result  : in out Upgrade_List);

   --  Sort the list of upgrade directories depending on the module dependencies.
   procedure Sort_Migration (List : in out Upgrade_List);

   --  Prepare the database migration described by the `Upgrade` record.
   --  Collect the SQL files according to the current database driver,
   --  sort them and return the list of absolute pathes in `Files`.
   procedure Prepare_Migration (Session : in ADO.Sessions.Session'Class;
                                Upgrade : in Upgrade_Type;
                                Files   : in out Util.Strings.Vectors.Vector);

   --  Run the database migration scripts described by the `Upgrade` record.
   --  If the `Files` is not empty and matches the `Upgrade` path, the list of
   --  files is used to perform the database migration.  Otherwise, `Prepare_Migration`
   --  is executed.  For each SQL file, read and execute each SQL statement
   --  by executing the `Execute` procedure.  At the end, the version associated
   --  with the module is updated to match the value defined in `Upgrade`.
   procedure Run_Migration (Session : in ADO.Sessions.Master_Session'Class;
                            Upgrade : in Upgrade_Type;
                            Files   : in out Util.Strings.Vectors.Vector;
                            Execute : not null
                              access procedure (Session : in ADO.Sessions.Session'Class;
                                                SQL : in String));

end ADO.Schemas.Databases;
