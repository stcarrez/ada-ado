with ADO;
with ADO.Drivers;
with ADO.Sessions;
with ADO.SQL;
with ADO.Sessions.Factory;
with Samples.User.Model;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with ADO.Statements;
with ADO.Databases;
with ADO.Queries;
with ADO.Queries.Loaders;

with Util.Log.Loggers;

with GNAT.Command_Line;

with Ada.Command_Line;
procedure Userdb is

   use ADO;
   use Ada;
   use Ada.Strings.Unbounded;
   use Samples.User.Model;
   use ADO.Statements;
   use GNAT.Command_Line;

   Factory    : ADO.Sessions.Factory.Session_Factory;

   User  : User_Ref;
   Users : User_Vector;
   Verbose : Boolean;

   procedure List_Users (Filter : in String);

   --  ------------------------------
   --  List users
   --  ------------------------------
   procedure List_Users (Filter : in String) is
      use User_Vectors;
      DB : ADO.Sessions.Session := Factory.Get_Session;
      Statement : ADO.Statements.Query_Statement := DB.Create_Statement (Filter);
      Query : ADO.SQL.Query;
   begin
      List (Object => Users, Session => DB, Query => Query);
      if Users.Is_Empty then
         Text_IO.Put_Line ("List is empty");
      else
         declare
            Iter : User_Vectors.Cursor := First (Users);
         begin
            while Has_Element (Iter) loop
               User := Element (Iter);
               Text_IO.Put_Line (Identifier'Image (User.Get_Id) & "     "
                                 & To_String (User.Get_Name)
                                 & "    " & To_String (User.Get_Email));
               User_Vectors.Next (Iter);
            end loop;
         end;
      end if;

      Statement := DB.Create_Statement ("select count(*) from user");
      Statement.Execute;
      if not Statement.Has_Elements then
         Text_IO.Put_Line ("Count query failed...");
      end if;
      declare
         Count : Integer := Statement.Get_Integer (0);
      begin
         Text_IO.Put_Line ("Count: " & Integer'Image (Count));
      end;
   end List_Users;

   --  ------------------------------
   --  List users
   --  ------------------------------
   procedure List_User_Info is
      use Samples.User.Model.User_Info_Vectors;
      DB      : ADO.Sessions.Session := Factory.Get_Session;
      Users   : Samples.User.Model.User_Info_Vector;
      Context : ADO.Queries.Context;
   begin
      Context.Set_Query (Samples.User.Model.Query_User_List);
      List (Object => Users, Session => DB, Context => Context);

      if Users.Is_Empty then
         Text_IO.Put_Line ("User info list is empty");
      else
         declare
            Iter : Cursor := First (Users);
            User : Samples.User.Model.User_Info;
         begin
            while Has_Element (Iter) loop
               User := Element (Iter);
               Text_IO.Put_Line (Identifier'Image (User.Id) & "     "
                                 & To_String (User.Name)
                                 & "    " & To_String (User.Email));
               Next (Iter);
            end loop;
         end;
      end if;
   end List_User_Info;

   procedure Initialize (File : in String) is
   begin
      Util.Log.Loggers.Initialize (File);
      ADO.Drivers.Initialize (File);
   end Initialize;

begin
   Initialize ("samples.properties");
   loop
      case Getopt ("v") is
         when ASCII.Nul =>
            exit;

         when 'v' =>
            Verbose := True;

         when others =>
            null;
      end case;
   end loop;

   Factory.Create (ADO.Drivers.Get_Config ("ado.database"));

   declare
      DB    : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
      Query : ADO.SQL.Query;
      Found : Boolean;
   begin
      DB.Begin_Transaction;
      List_User_Info;

      List_Users (Filter => "");

      loop
         declare
            Name : constant String := Get_Argument;
         begin
            exit when Name = "";

            Query.Bind_Param (1, Name);
            Query.Set_Filter ("name = ?");
            User.Find (Session => DB, Query => Query, Found => Found);
            if User.Is_Null or not Found then
               User.Set_Name (Name);
               User.Set_Email (Name & "@gmail.com");
               User.Set_Description ("My friend " & Name);
               --  User.Set_Password ("my password");
               User.Set_Status (0);
               User.Save (DB);

               Text_IO.Put_Line ("User created: " & Identifier'Image (User.Get_Id));
            else
               Text_IO.Put_Line ("User " & Name & ": " & Identifier'Image (User.Get_Id));
            end if;
         end;
      end loop;
      DB.Rollback;

   exception
      when ADO.Sessions.NOT_FOUND =>
         Text_IO.Put_Line ("User 23 does not exist");
   end;
   Text_IO.Put_Line ("Exiting");
end Userdb;
