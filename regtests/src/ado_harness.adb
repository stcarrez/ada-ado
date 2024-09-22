-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2009, 2010, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ADO.Testsuite;
with ADO.Drivers;
with Regtests;

with Util.Tests;
with Util.Properties;
procedure ADO_Harness is

   procedure Initialize (Props : in Util.Properties.Manager);

   procedure Harness is new Util.Tests.Harness (ADO.Testsuite.Suite,
                                               Initialize);

   --  ------------------------------
   --  Initialization procedure: setup the database
   --  ------------------------------
   procedure Initialize (Props : in Util.Properties.Manager) is
      DB : constant String := Props.Get ("test.database",
                                         "sqlite:///regtests.db");
   begin
      ADO.Drivers.Initialize (Props);
      Regtests.Initialize (DB);
   end Initialize;

begin
   Harness ("ado-tests.xml");
end ADO_Harness;
