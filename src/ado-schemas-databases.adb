-----------------------------------------------------------------------
--  ado-schemas-databases -- Database creation
--  Copyright (C) 2018, 2019 Stephane Carrez
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

with ADO.Connections;
package body ADO.Schemas.Databases is

   --  ------------------------------
   --  Create the database and initialize it with the schema SQL file.
   --  ------------------------------
   procedure Create_Database (Admin       : in ADO.Sessions.Sources.Data_Source'Class;
                              Config      : in ADO.Sessions.Sources.Data_Source'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector) is
      Name   : constant String := Config.Get_Driver;
      Driver : constant Connections.Driver_Access := Connections.Get_Driver (Name);
   begin
      Messages.Clear;
      Driver.Create_Database (Admin       => Admin,
                              Config      => Config,
                              Schema_Path => Schema_Path,
                              Messages    => Messages);
   end Create_Database;

end ADO.Schemas.Databases;
