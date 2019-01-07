-----------------------------------------------------------------------
--  ado-datasets-tests -- Test executing queries and using datasets
--  Copyright (C) 2013, 2014, 2015, 2017, 2019 Stephane Carrez
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
with Util.Test_Caller;

with Regtests.Simple.Model;
with Regtests.Statements.Model;

with ADO.Queries.Loaders;
package body ADO.Datasets.Tests is

   package Caller is new Util.Test_Caller (Test, "ADO.Datasets");

   package User_List_Query_File is
     new ADO.Queries.Loaders.File (Path => "regtests/files/user-list.xml",
                                   Sha1 => "");

   package User_List_Query is
     new ADO.Queries.Loaders.Query (Name => "user-list",
                                    File => User_List_Query_File.File'Access);

   package User_List_Count_Query is
     new ADO.Queries.Loaders.Query (Name => "user-list-count",
                                    File => User_List_Query_File.File'Access);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ADO.Datasets.List (from <sql>)",
                       Test_List'Access);
      Caller.Add_Test (Suite, "Test ADO.Datasets.Get_Count (from <sql>)",
                       Test_Count'Access);
      Caller.Add_Test (Suite, "Test ADO.Datasets.Get_Count (from <sql-count>)",
                       Test_Count_Query'Access);
      Caller.Add_Test (Suite, "Test ADO.Datasets.List (<sql> with null)",
                       Test_List_Nullable'Access);
   end Add_Tests;

   procedure Test_List (T : in out Test) is
      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Query  : ADO.Queries.Context;
      Count  : Natural;
      Data   : ADO.Datasets.Dataset;
   begin
      Query.Set_Count_Query (User_List_Query.Query'Access);
      Query.Bind_Param ("filter", String '("test-list"));
      Count := ADO.Datasets.Get_Count (DB, Query);
      for I in 1 .. 100 loop
         declare
            User : Regtests.Simple.Model.User_Ref;
         begin
            User.Set_Name ("John " & Integer'Image (I));
            User.Set_Select_Name ("test-list");
            User.Set_Value (ADO.Identifier (I));
            User.Save (DB);
         end;
      end loop;
      DB.Commit;

      Query.Set_Query (User_List_Query.Query'Access);
      ADO.Datasets.List (Data, DB, Query);
      Util.Tests.Assert_Equals (T, 100 + Count, Data.Get_Count, "Invalid dataset size");
   end Test_List;

   --  ------------------------------
   --  Test dataset lists with null columns.
   --  ------------------------------
   procedure Test_List_Nullable (T : in out Test) is
      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Query  : ADO.Queries.Context;
      Count  : Natural;
      Data   : ADO.Datasets.Dataset;
   begin
      Query.Set_SQL ("SELECT COUNT(*) FROM test_nullable_table");
      Count := ADO.Datasets.Get_Count (DB, Query);

      for I in 1 .. 100 loop
         declare
            Item : Regtests.Statements.Model.Nullable_Table_Ref;
         begin
            Item.Set_Bool_Value ((Value => False, Is_Null => False));
            if (I mod 2) = 0 then
               Item.Set_Int_Value (ADO.Nullable_Integer '(123, False));
            end if;
            if (I mod 4) = 0 then
               Item.Set_Time_Value (ADO.Nullable_Time '(Value => Ada.Calendar.Clock,
                                                        Is_Null => False));
            end if;
            Item.Save (DB);
         end;
      end loop;
      DB.Commit;

      Query.Set_SQL ("SELECT * FROM test_nullable_table");
      ADO.Datasets.List (Data, DB, Query);
      Util.Tests.Assert_Equals (T, 100 + Count, Data.Get_Count, "Invalid dataset size");
   end Test_List_Nullable;

   procedure Test_Count (T : in out Test) is
      DB     : constant ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Query  : ADO.Queries.Context;
      Count  : Natural;
   begin
      Query.Set_Query (User_List_Count_Query.Query'Access);
      Count := ADO.Datasets.Get_Count (DB, Query);
      T.Assert (Count > 0,
                "The ADO.Datasets.Get_Count query should return a positive count");
   end Test_Count;

   procedure Test_Count_Query (T : in out Test) is
      DB     : constant ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Query  : ADO.Queries.Context;
      Count  : Natural;
   begin
      Query.Set_Count_Query (User_List_Query.Query'Access);
      Query.Bind_Param ("filter", String '("test-list"));
      Count := ADO.Datasets.Get_Count (DB, Query);
      T.Assert (Count > 0,
                "The ADO.Datasets.Get_Count query should return a positive count");
   end Test_Count_Query;

end ADO.Datasets.Tests;
