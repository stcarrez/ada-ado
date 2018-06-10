-----------------------------------------------------------------------
--  ADO Sequences -- Database sequence generator
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2017, 2018 Stephane Carrez
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

with Ada.Exceptions;
with Ada.Calendar;

with ADO.Statements;
with ADO.Objects;
with ADO.Sessions;
with ADO.Utils;
with Regtests;

with Regtests.Simple.Model;
with Regtests.Images.Model;

with Util.Assertions;
with Util.Measures;
with Util.Log;
with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Test_Caller;

package body ADO.Tests is

   use Ada.Exceptions;
   use ADO.Statements;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Tests");

   package Caller is new Util.Test_Caller (Test, "ADO");

   procedure Assert_Has_Message (T : in Test;
                                 E : in Exception_Occurrence);

   procedure Assert_Has_Message (T : in Test;
                                 E : in Exception_Occurrence) is
      Message : constant String := Exception_Message (E);
   begin
      Log.Info ("Exception: {0}", Message);
      T.Assert (Message'Length > 0,
                "Exception " & Exception_Name (E) & " does not have any message");
   end Assert_Has_Message;

   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;

   --  ------------------------------
   --  Check:
   --     Object_Ref.Load
   --     Object_Ref.Is_Null
   --  ------------------------------
   procedure Test_Load (T : in out Test) is

      DB  : ADO.Sessions.Session := Regtests.Get_Database;

      Object : Regtests.Simple.Model.User_Ref;
   begin
      T.Assert (Object.Is_Null, "Object_Ref.Is_Null: Empty object must be null");

      Object.Load (DB, -1);
      T.Assert (False, "Object_Ref.Load: Load must raise NOT_FOUND exception");

   exception
      when ADO.Objects.NOT_FOUND =>
         T.Assert (Object.Is_Null, "Object_Ref.Load: Must not change the object");
   end Test_Load;

   --  ------------------------------
   --  Check:
   --    Object_Ref.Load
   --    Object_Ref.Save
   --    <Model>.Set_xxx (Unbounded_String)
   --    <Model>.Create
   --  ------------------------------
   procedure Test_Create_Load (T : in out Test) is

      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;

      Object : Regtests.Simple.Model.User_Ref;
      Check  : Regtests.Simple.Model.User_Ref;
   begin
      --  Initialize and save an object
      Object.Set_Name ("A simple test name");
      Object.Save (DB);

      T.Assert (Object.Get_Id > 0, "Saving an object did not allocate an identifier");

      --  Load the object
      Check.Load (DB, Object.Get_Id);
      T.Assert (not Check.Is_Null, "Object_Ref.Load: Loading the object failed");
   end Test_Create_Load;

   --  ------------------------------
   --  Check:
   --    Various error checks on database connections
   --
   --    Master_Connection.Rollback
   --  ------------------------------
   procedure Test_Not_Open (T : in out Test) is
      DB : ADO.Sessions.Master_Session;
   begin
      begin
         DB.Rollback;
         T.Fail ("Master_Connection.Rollback should raise an exception");

      exception
         when E : ADO.Sessions.NOT_OPEN =>
            Assert_Has_Message (T, E);
      end;

      begin
         DB.Commit;
         T.Fail ("Master_Connection.Commit should raise an exception");

      exception
         when E : ADO.Sessions.NOT_OPEN =>
            Assert_Has_Message (T, E);
      end;

   end Test_Not_Open;

   --  ------------------------------
   --  Check id generation
   --  ------------------------------
   procedure Test_Allocate (T : in out Test) is
      DB     : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Key    : ADO.Objects.Object_Key (Of_Type => ADO.Objects.KEY_INTEGER,
                                       Of_Class => Regtests.Simple.Model.ALLOCATE_TABLE);
      PrevId : Identifier := NO_IDENTIFIER;
      S      : Util.Measures.Stamp;
   begin
      for I in 1 .. 200 loop
         declare
            Obj : Regtests.Simple.Model.Allocate_Ref;
         begin
            Obj.Save (DB);
            Key := Obj.Get_Key;
            if PrevId /= NO_IDENTIFIER then
               T.Assert (Objects.Get_Value (Key) = PrevId + 1, "Invalid allocated identifier: "
                         & Objects.To_String (Key) & " previous=" & Identifier'Image (PrevId));
            end if;
            PrevId := Objects.Get_Value (Key);
         end;
      end loop;
      Util.Measures.Report (S, "Allocate 200 ids");
   end Test_Allocate;

   --  ------------------------------
   --  Check:
   --    Object.Save (with creation)
   --    Object.Find
   --    Object.Save (update)
   --  ------------------------------
   procedure Test_Create_Save (T : in out Test) is
      use type ADO.Sessions.Connection_Status;

      DB   : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Ref  : Regtests.Simple.Model.Allocate_Ref;
      Ref2 : Regtests.Simple.Model.Allocate_Ref;
   begin
      T.Assert (DB.Get_Status = ADO.Sessions.OPEN,
                "The database connection is open");

      Ref.Set_Name ("Testing the allocation");
      Ref.Save (DB);
      T.Assert (Ref.Get_Id > 0, "Object must have an id");

      Ref.Set_Name ("Testing the allocation: update");
      Ref.Save (DB);

      Ref2.Load (DB, Ref.Get_Id);

   end Test_Create_Save;

   --  ------------------------------
   --  Check:
   --    Object.Save (with creation)
   --  ------------------------------
   procedure Test_Perf_Create_Save (T : in out Test) is
      DB : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      S  : Util.Measures.Stamp;
   begin
      DB.Begin_Transaction;
      for I in 1 .. 1_000 loop
         declare
            Ref  : Regtests.Simple.Model.Allocate_Ref;
         begin
            Ref.Set_Name ("Testing the allocation");
            Ref.Save (DB);
            T.Assert (Ref.Get_Id > 0, "Object must have an id");
         end;
      end loop;
      DB.Commit;
      Util.Measures.Report (S, "Create 1000 rows");
   end Test_Perf_Create_Save;

   procedure Test_Delete_All (T : in out Test) is

      DB : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Stmt : ADO.Statements.Delete_Statement
        := DB.Create_Statement (Regtests.Simple.Model.ALLOCATE_TABLE);
      Result : Natural;
   begin
      DB.Begin_Transaction;
      Stmt.Execute (Result);
      Log.Info ("Deleted {0} rows", Natural'Image (Result));
      DB.Commit;

      T.Assert (Result > 100, "Too few rows were deleted");
   end Test_Delete_All;

   --  ------------------------------
   --  Test string insert.
   --  ------------------------------
   procedure Test_String (T : in out Test) is
      use Ada.Strings.Unbounded;

      DB   : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      User : Regtests.Simple.Model.User_Ref;
      Usr2 : Regtests.Simple.Model.User_Ref;
      Name : Unbounded_String;
   begin
      for I in 1 .. 127 loop
         Append (Name, Character'Val (I));
      end loop;
      Append (Name, ' ');
      Append (Name, ' ');
      Append (Name, ' ');
      Append (Name, ' ');
      DB.Begin_Transaction;
      User.Set_Name (Name);
      User.Save (DB);
      DB.Commit;

      --  Check that we can load the image and the blob.
      Usr2.Load (DB, User.Get_Id);
      Util.Tests.Assert_Equals (T, To_String (Name), String '(Usr2.Get_Name),
                                "Invalid name inserted for user");
   end Test_String;

   --  ------------------------------
   --  Test blob insert.
   --  ------------------------------
   procedure Test_Blob (T : in out Test) is
      use Ada.Streams;

      procedure Assert_Equals is
        new Util.Assertions.Assert_Equals_T (Ada.Streams.Stream_Element);

      DB   : ADO.Sessions.Master_Session := Regtests.Get_Master_Database;
      Img  : Regtests.Images.Model.Image_Ref;
      Size : constant Natural := 100;
      Data : ADO.Blob_Ref := ADO.Create_Blob (Size);
      Img2 : Regtests.Images.Model.Image_Ref;
   begin
      for I in 1 .. Size loop
         Data.Value.Data (Ada.Streams.Stream_Element_Offset (I)) := Integer'Pos ((64 + I) mod 255);
      end loop;
      DB.Begin_Transaction;
      Img.Set_Image (Data);
      Img.Set_Create_Date (Ada.Calendar.Clock);
      Img.Save (DB);
      DB.Commit;

      --  Check that we can load the image and the blob.
      Img2.Load (DB, Img.Get_Id);
      T.Assert (Img2.Get_Image.Is_Null = False, "No image blob loaded");

      --  And verify that the blob data matches what we inserted.
      Util.Tests.Assert_Equals (T, Size, Integer (Img2.Get_Image.Value.Len),
                                "Invalid blob length");
      for I in 1 .. Data.Value.Len loop
         Assert_Equals (T, Data.Value.Data (I), Img2.Get_Image.Value.Data (I),
                        "Invalid blob content at " & Stream_Element_Offset'Image (I));
      end loop;

      --  Create a blob initialized with a file content.
      Data := ADO.Create_Blob ("Makefile");
      T.Assert (not Data.Is_Null, "Null blob returned by Create_Blob");
      T.Assert (Data.Value.Len > 100, "Blob length initialized from file is too small");

      declare
         Content : Ada.Streams.Stream_Element_Array (1 .. 10);
         Img3    : Regtests.Images.Model.Image_Ref;
      begin
         for I in Content'Range loop
            Content (I) := Ada.Streams.Stream_Element_Offset'Pos (I + 30);
         end loop;
         Data := ADO.Create_Blob (Content);
         T.Assert (not Data.Is_Null, "Null blob returned by Create_Blob (Stream_Element_Array)");
         T.Assert (Data.Value.Len = 10, "Blob length initialized from array is too small");

         DB.Begin_Transaction;
         Img3.Set_Image (Data);
         Img3.Set_Create_Date (Ada.Calendar.Clock);
         Img3.Save (DB);
         DB.Commit;

         --  Check that we can load the image and the blob.
         Img2.Load (DB, Img3.Get_Id);
         T.Assert (Img2.Get_Image.Is_Null = False, "No image blob loaded");

         Img2.Set_Image (ADO.Null_Blob);
         Img2.Save (DB);
         DB.Commit;
      end;
   end Test_Blob;

   --  ------------------------------
   --  Test the To_Object and To_Identifier operations.
   --  ------------------------------
   procedure Test_Identifier_To_Object (T : in out Test) is
      Val : Util.Beans.Objects.Object := ADO.Utils.To_Object (ADO.NO_IDENTIFIER);
   begin
      T.Assert (Util.Beans.Objects.Is_Null (Val),
                "To_Object must return null for ADO.NO_IDENTIFIER");
      T.Assert (ADO.Utils.To_Identifier (Val) = ADO.NO_IDENTIFIER,
                "To_Identifier must return ADO.NO_IDENTIFIER for null");
      Val := ADO.Utils.To_Object (1);
      T.Assert (not Util.Beans.Objects.Is_Null (Val),
                "To_Object must not return null for a valid Identifier");
      T.Assert (ADO.Utils.To_Identifier (Val) = 1,
                "To_Identifier must return the correct identifier");
   end Test_Identifier_To_Object;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Object_Ref.Load", Test_Load'Access);
      Caller.Add_Test (Suite, "Test Object_Ref.Save", Test_Create_Load'Access);
      Caller.Add_Test (Suite, "Test Master_Connection init error", Test_Not_Open'Access);
      Caller.Add_Test (Suite, "Test Sequences.Factory", Test_Allocate'Access);
      Caller.Add_Test (Suite, "Test Object_Ref.Save/Create/Update",
                       Test_Create_Save'Access);
      Caller.Add_Test (Suite, "Test Object_Ref.Create (DB Insert)",
                       Test_Perf_Create_Save'Access);
      Caller.Add_Test (Suite, "Test Statement.Delete_Statement (delete all)",
                       Test_Delete_All'Access);
      Caller.Add_Test (Suite, "Test insert string",
                       Test_String'Access);
      Caller.Add_Test (Suite, "Test insert blob",
                       Test_Blob'Access);
      Caller.Add_Test (Suite, "Test ADO.Utils.To_Object/To_Identifier",
                       Test_Identifier_To_Object'Access);
   end Add_Tests;

end ADO.Tests;
