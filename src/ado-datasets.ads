-----------------------------------------------------------------------
--  ado-datasets -- Datasets
--  Copyright (C) 2013, 2014 Stephane Carrez
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
