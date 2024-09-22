-----------------------------------------------------------------------
--  regtests -- Support for unit tests
--  Copyright (C) 2009, 2010, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Sessions.Factory;

package body Regtests is

   Factory    : aliased ADO.Sessions.Factory.Session_Factory;
   Controller : aliased ADO.Sessions.Sources.Data_Source;

   --  ------------------------------
   --  Get the database manager to be used for the unit tests
   --  ------------------------------
   function Get_Controller return ADO.Sessions.Sources.Data_Source'Class is
   begin
      return Controller;
   end Get_Controller;

   --  ------------------------------
   --  Get the readonly connection database to be used for the unit tests
   --  ------------------------------
   function Get_Database return ADO.Sessions.Session is
   begin
      return Factory.Get_Session;
   end Get_Database;

   --  ------------------------------
   --  Get the writeable connection database to be used for the unit tests
   --  ------------------------------
   function Get_Master_Database return ADO.Sessions.Master_Session is
   begin
      return Factory.Get_Master_Session;
   end Get_Master_Database;

   --  ------------------------------
   --  Set the audit manager on the factory.
   --  ------------------------------
   procedure Set_Audit_Manager (Manager : in ADO.Audits.Audit_Manager_Access) is
   begin
      Factory.Set_Audit_Manager (Manager);
   end Set_Audit_Manager;

   --  ------------------------------
   --  Initialize the test database
   --  ------------------------------
   procedure Initialize (Name : in String) is
   begin
      Controller.Set_Connection (Name);
      Factory.Create (Controller);
   end Initialize;

end Regtests;
