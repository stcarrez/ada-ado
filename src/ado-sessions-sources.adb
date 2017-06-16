-----------------------------------------------------------------------
--  ADO Databases -- Database Connections
--  Copyright (C) 2010, 2011, 2012, 2013, 2017 Stephane Carrez
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

package body ADO.Sessions.Sources is

   use ADO.Drivers;
   use type ADO.Drivers.Connections.Database_Connection_Access;

   --  ------------------------------
   --  Set the master data source
   --  ------------------------------
   procedure Set_Master (Controller : in out Replicated_DataSource;
                         Master     : in Data_Source_Access) is
   begin
      Controller.Master := Master;
   end Set_Master;

   --  ------------------------------
   --  Get the master data source
   --  ------------------------------
   function Get_Master (Controller : in Replicated_DataSource)
                       return Data_Source_Access is
   begin
      return Controller.Master;
   end Get_Master;

   --  ------------------------------
   --  Set the slave data source
   --  ------------------------------
   procedure Set_Slave (Controller : in out Replicated_DataSource;
                        Slave      : in Data_Source_Access) is
   begin
      Controller.Slave := Slave;
   end Set_Slave;

   --  ------------------------------
   --  Get the slave data source
   --  ------------------------------
   function Get_Slave (Controller : in Replicated_DataSource)
                      return Data_Source_Access is
   begin
      return Controller.Slave;
   end Get_Slave;

   --  ------------------------------
   --  Get a slave database connection
   --  ------------------------------
--   function Get_Slave_Connection (Controller : in Replicated_DataSource)
--                                 return Connection'Class is
--   begin
--      return Controller.Slave.Get_Connection;
--   end Get_Slave_Connection;

end ADO.Sessions.Sources;
