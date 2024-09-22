-----------------------------------------------------------------------
--  ado-datasets -- Datasets
--  Copyright (C) 2013, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects.Datasets;

with ADO.Sessions;
with ADO.Queries;

package ADO.Datasets is

   subtype Dataset is Util.Beans.Objects.Datasets.Dataset;

   --  Execute the SQL query on the database session and populate the dataset.
   --  The column names are used to setup the dataset row bean definition.
   procedure List (Into    : in out Dataset;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.Queries.Context'Class);

   --  Get the number of items in a list by executing an SQL query.
   function Get_Count (Session : in ADO.Sessions.Session'Class;
                       Query   : in ADO.Queries.Context'Class) return Natural;

end ADO.Datasets;
