-----------------------------------------------------------------------
--  ADO Databases -- Database Connections
--  Copyright (C) 2010, 2011, 2012, 2013, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ADO.Sessions.Sources is

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
