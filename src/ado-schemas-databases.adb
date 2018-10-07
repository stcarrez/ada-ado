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

with ADO.Drivers.Connections;
package body ADO.Schemas.Databases is

   --  ------------------------------
   --  Create the database and initialize it with the schema SQL file.
   --  ------------------------------
   procedure Create_Database (Session     : in out Sessions.Master_Session;
                              Config      : in Configs.Configuration'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector) is
      procedure Process (Database : in out ADO.Drivers.Connections.Database_Connection'Class);

      procedure Process (Database : in out ADO.Drivers.Connections.Database_Connection'Class) is
      begin
         Database.Create_Database (Config      => Config,
                                   Schema_Path => Schema_Path,
                                   Messages    => Messages);
      end Process;
   begin
      Session.Access_Connection (Process'Access);
   end Create_Database;

end ADO.Schemas.Databases;
