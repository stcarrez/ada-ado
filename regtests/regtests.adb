-----------------------------------------------------------------------
--  regtests -- Support for unit tests
--  Copyright (C) 2009, 2010, 2017, 2018 Stephane Carrez
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
