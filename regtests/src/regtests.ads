-----------------------------------------------------------------------
--  regtests -- Support for unit tests
--  Copyright (C) 2009, 2010, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Audits;
with ADO.Sessions;
with ADO.Sessions.Sources;
package Regtests is

   --  Get the database manager to be used for the unit tests
   function Get_Controller return ADO.Sessions.Sources.Data_Source'Class;

   --  Get the readonly connection database to be used for the unit tests
   function Get_Database return ADO.Sessions.Session;

   --  Get the writeable connection database to be used for the unit tests
   function Get_Master_Database return ADO.Sessions.Master_Session;

   --  Set the audit manager on the factory.
   procedure Set_Audit_Manager (Manager : in ADO.Audits.Audit_Manager_Access);

   --  Initialize the test database
   procedure Initialize (Name : in String);

end Regtests;
