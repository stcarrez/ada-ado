-----------------------------------------------------------------------
--  ado-schemas-databases -- Database creation and upgrade
--  Copyright (C) 2018, 2019, 2022, 2023 Stephane Carrez
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
with Ada.Directories;
with Ada.Characters.Handling;

with Util.Log.Loggers;
with Util.Files;
with Util.Strings;
with ADO.SQL;
with ADO.Model;
with ADO.Connections;
with ADO.Statements;
package body ADO.Schemas.Databases is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Schemas.Databases");

   function Is_Space (C : in Character) return Boolean
     is (Ada.Characters.Handling.Is_Space (C)
           or else Ada.Characters.Handling.Is_Line_Terminator (C));

   function Depend_Version (Dep : in String; Name : in String) return Natural;

   function Depend_Version (Dep : in String; Name : in String) return Natural is
      Pos : Positive := Dep'First;
      Sep1 : Natural;
      Sep2 : Natural;
   begin
      while Pos <= Dep'Last loop
         while Is_Space (Dep (Pos)) and then Pos <= Dep'Last loop
            Pos := Pos + 1;
         end loop;
         exit when Pos > Dep'Last;
         Sep1 := Util.Strings.Index (Dep, ':', Pos);
         exit when Sep1 = 0;
         Sep2 := Util.Strings.Index (Dep, ' ', Sep1);
         if Sep2 = 0 then
            Sep2 := Dep'Last;
         else
            Sep2 := Sep2 - 1;
         end if;
         if Dep (Pos .. Sep1 - 1) = Name then
            begin
               return Natural'Value (Dep (Sep1 + 1 .. Sep2));

            exception
               when Constraint_Error =>
                  return 0;
            end;
         end if;
         Pos := Sep2 + 1;
      end loop;
      return 0;
   end Depend_Version;

   function "<" (Left, Right : in Upgrade_Type) return Boolean is
   begin
      if Left.Name = Right.Name then
         return Left.Version < Right.Version;
      end if;

      declare
         Left_Depend : constant String := To_String (Left.Depend);
         Right_Depend : constant String := To_String (Right.Depend);
         D1, D2 : Natural;
      begin
         D1 := Depend_Version (Right_Depend, To_String (Left.Name));
         if D1 > 0 then
            if Left.Version < D1 then
               return True;
            elsif Left.Version > D1 then
               return False;
            end if;
         end if;

         D2 := Depend_Version (Left_Depend, To_String (Right.Name));
         if D2 > 0 then
            if D2 > Right.Version then
               return False;
            elsif D2 < Right.Version then
               return True;
            elsif D1 > 0 then
               return False;
            end if;
         elsif D1 > 0 then
            return True;
         end if;

         if Left_Depend'Length /= Right_Depend'Length then
            return Left_Depend'Length < Right_Depend'Length;
         end if;
      end;

      return Left.Name < Right.Name;
   end "<";

   --  ------------------------------
   --  Create the database and initialize it with the schema SQL file.
   --  ------------------------------
   procedure Create_Database (Admin       : in ADO.Sessions.Sources.Data_Source'Class;
                              Config      : in ADO.Sessions.Sources.Data_Source'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector) is
      use type Connections.Driver_Access;

      Name   : constant String := Config.Get_Driver;
      Driver : constant Connections.Driver_Access := Connections.Get_Driver (Name);
   begin
      if Driver = null then
         raise ADO.Configs.Connection_Error with "Database driver '" & Name & "' not found";
      end if;
      Messages.Clear;
      Driver.Create_Database (Admin       => Admin,
                              Config      => Config,
                              Schema_Path => Schema_Path,
                              Messages    => Messages);
   end Create_Database;

   --  ------------------------------
   --  Scan the directory for migration script and check with the database to define
   --  the database upgrade scripts that must be executed.  The result list must then
   --  be sorted by using `Sort_Migration` to honor the module dependencies.
   --  ------------------------------
   procedure Scan_Migration (Session : in ADO.Sessions.Session'Class;
                             Path    : in String;
                             Result  : in out Upgrade_List) is
      use Ada.Directories;

      function Get_Module_Version (Name : in String) return Natural;
      procedure Module_Migration (Path : in String;
                                  Name : in String);
      procedure Add_Migration (Path : in String;
                               Name : in String;
                               Version : in Positive);

      function Is_Number (Item : in String) return Boolean
         is (for all C of Item => C in '0' .. '9');

      --  Special case for ado module migration.  If ado_version does not exist,
      --  we must not run the SELECT on ado_version to get a module version and
      --  we want to proceed.
      Has_Version_Table : constant Boolean := Session.Has_Table ("ado_version");

      procedure Add_Migration (Path : in String;
                               Name : in String;
                               Version : in Positive) is
         Upgrade : Upgrade_Type;
      begin
         Upgrade.Name := To_Unbounded_String (Name);
         Upgrade.Path := To_Unbounded_String (Path);
         Upgrade.Version := Version;
         declare
            procedure Read (Line : in String);

            Dep_Path : constant String := Util.Files.Compose (Path, "depend.conf");

            procedure Read (Line : in String) is
               First : Natural := Line'First;
               Last  : Natural := Line'Last;
            begin
               while First <= Last and then Is_Space (Line (First)) loop
                  First := First + 1;
               end loop;
               while Last >= First and then Is_Space (Line (Last)) loop
                  Last := Last - 1;
               end loop;
               if First < Last then
                  if Length (Upgrade.Depend) > 0 then
                     Append (Upgrade.Depend, " ");
                  end if;
                  Append (Upgrade.Depend, Line (First .. Last));
               end if;
            end Read;
         begin
            if Ada.Directories.Exists (Dep_Path) then
               Util.Files.Read_File (Dep_Path, Read'Access);
            end if;
         end;
         Result.Append (Upgrade);
      end Add_Migration;

      function Get_Module_Version (Name : in String) return Natural is
      begin
         if not Has_Version_Table then
            return 0;
         end if;
         declare
            Stmt : ADO.Statements.Query_Statement
              := Session.Create_Statement ("SELECT version FROM ado_version WHERE name = ?");
         begin
            Stmt.Add_Param (Name);
            Stmt.Execute;
            return Stmt.Get_Result_Integer;
         end;
      end Get_Module_Version;

      procedure Module_Migration (Path : in String;
                                  Name : in String) is
         Current_Version : constant Natural := Get_Module_Version (Name);
         Filter  : constant Filter_Type := (Ordinary_File => False,
                                            Directory     => True,
                                            others        => False);
         Search  : Search_Type;
         Ent     : Ada.Directories.Directory_Entry_Type;
      begin
         Start_Search (Search, Directory => Path,
                      Pattern => "*", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Upgrade : constant String := Ada.Directories.Simple_Name (Ent);
               Version : Natural;
            begin
               --  The directory is a number that indicates the upgraded version.
               if Is_Number (Upgrade) then
                  Version := Natural'Value (Upgrade);
                  if Version > Current_Version then
                     Add_Migration (Ada.Directories.Full_Name (Ent), Name, Version);
                  end if;
               end if;
            end;
         end loop;
      end Module_Migration;

      --  Look only at directories, each directory name indicates a database module.
      Filter  : constant Filter_Type := (Ordinary_File => False,
                                         Directory     => True,
                                         others        => False);
      Search  : Search_Type;
      Ent     : Ada.Directories.Directory_Entry_Type;
   begin
      Log.Debug ("Scan directory {0} for database migration", Path);

      Start_Search (Search, Directory => Path,
                    Pattern => "*", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String := Ada.Directories.Simple_Name (Ent);
         begin
            if Name /= "." and then Name /= ".." then
               Module_Migration (Ada.Directories.Full_Name (Ent), Name);
            end if;
         end;
      end loop;

      Log.Info ("Scan migration found{0} upgrades in {1}", Result.Length'Image, Path);
   end Scan_Migration;

   --  ------------------------------
   --  Sort the list of upgrade directories depending on the module dependencies.
   --  ------------------------------
   procedure Sort_Migration (List : in out Upgrade_List) is
      function Find (Name : in String; Version : in Positive) return String;
      procedure Add_Dependencies (Upgrade : in out Upgrade_Type;
                                  Depend  : in String);
      procedure Update_Dependencies (Upgrade : in out Upgrade_Type);

      package Sort is
         new Upgrade_Lists.Generic_Sorting ("<" => "<");

      function Find (Name : in String; Version : in Positive) return String is
      begin
         for Upgrade of List loop
            if Upgrade.Version = Version and then Upgrade.Name = Name then
               return To_String (Upgrade.Depend);
            end if;
         end loop;
         return "";
      end Find;

      procedure Add_Dependencies (Upgrade : in out Upgrade_Type;
                                  Depend  : in String) is
         Pos    : Positive := Depend'First;
         Sep1   : Natural;
         Sep2   : Natural;
      begin
         while Pos <= Depend'Last loop
            while Is_Space (Depend (Pos)) and then Pos <= Depend'Last loop
               Pos := Pos + 1;
            end loop;
            exit when Pos > Depend'Last;
            Sep1 := Util.Strings.Index (Depend, ':', Pos);
            exit when Sep1 = 0;
            Sep2 := Util.Strings.Index (Depend, ' ', Sep1);
            if Sep2 = 0 then
               Sep2 := Depend'Last;
            else
               Sep2 := Sep2 - 1;
            end if;
            declare
               Version : constant Natural
                 := Depend_Version (To_String (Upgrade.Depend), Depend (Pos .. Sep1 - 1));
            begin
               if Version = 0 then
                  Append (Upgrade.Depend, " ");
                  Append (Upgrade.Depend, Depend (Pos .. Sep2));

                  Log.Debug ("Add dependency {0} to {1}, now using {2}",
                             Depend (Pos .. Sep2), To_String (Upgrade.Name),
                             To_String (Upgrade.Depend));
               end if;
            end;
            Pos := Sep2 + 1;
         end loop;
      end Add_Dependencies;

      procedure Update_Dependencies (Upgrade : in out Upgrade_Type) is
         Depend : constant String := To_String (Upgrade.Depend);
         Pos    : Positive := Depend'First;
         Sep1   : Natural;
         Sep2   : Natural;
      begin
         while Pos <= Depend'Last loop
            while Is_Space (Depend (Pos)) and then Pos <= Depend'Last loop
               Pos := Pos + 1;
            end loop;
            exit when Pos > Depend'Last;
            Sep1 := Util.Strings.Index (Depend, ':', Pos);
            exit when Sep1 = 0;
            Sep2 := Util.Strings.Index (Depend, ' ', Sep1);
            if Sep2 = 0 then
               Sep2 := Depend'Last;
            else
               Sep2 := Sep2 - 1;
            end if;

            --  Add the dependencies defined for the component+version we depend on.
            declare
               Version : Positive;
            begin
               Version := Positive'Value (Depend (Sep1 + 1 .. Sep2));
               Add_Dependencies (Upgrade, Find (Depend (Pos .. Sep1 - 1), Version));

            exception
               when Constraint_Error =>
                  null;
            end;
            Pos := Sep2 + 1;
         end loop;
      end Update_Dependencies;

   begin
      for Upgrade of List loop
         Update_Dependencies (Upgrade);
      end loop;

      Sort.Sort (List);
   end Sort_Migration;

   --  ------------------------------
   --  Prepare the database migration described by the `Upgrade` record.
   --  Collect the SQL files according to the current database driver,
   --  sort them and return the list of absolute pathes in `Files`.
   --  ------------------------------
   procedure Prepare_Migration (Session : in ADO.Sessions.Session'Class;
                                Upgrade : in Upgrade_Type;
                                Files   : in out Util.Strings.Vectors.Vector) is
      use Ada.Directories;

      package Sort is
         new Util.Strings.Vectors.Generic_Sorting ("<");

      Filter  : constant Filter_Type := (Ordinary_File => True,
                                         Directory     => False,
                                         others        => False);
      Path    : constant String := To_String (Upgrade.Path);
      Driver  : constant String := Session.Get_Driver.Get_Driver_Name & "-";
      Search  : Search_Type;
      Ent     : Ada.Directories.Directory_Entry_Type;
   begin
      Log.Debug ("Prepare SQL migration list from directory {0}", Path);

      Files.Clear;
      Start_Search (Search, Directory => Path,
                    Pattern => "*.sql", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Name : constant String := Ada.Directories.Simple_Name (Ent);
            File_Path : constant String := Ada.Directories.Full_Name (Ent);
            Pos  : Natural;
         begin
            Pos := Util.Strings.Index (Name, '-');
            if Pos = 0 or else Util.Strings.Starts_With (Name (Pos + 1 .. Name'Last), Driver)
              or else Util.Strings.Starts_With (Name (Pos + 1 .. Name'Last), "all-")
            then
               Files.Append (File_Path);
            end if;
         end;
      end loop;

      Sort.Sort (Files);
      Log.Info ("Prepare migration found{0} SQL files in {1}", Files.Length'Image, Path);
   end Prepare_Migration;

   --  ------------------------------
   --  Run the database migration scripts described by the `Upgrade` record.
   --  If the `Files` is not empty and matches the `Upgrade` path, the list of
   --  files is used to perform the database migration.  Otherwise, `Prepare_Migration`
   --  is executed.  For each SQL file, read and execute each SQL statement
   --  by executing the `Execute` procedure.  At the end, the version associated
   --  with the module is updated to match the value defined in `Upgrade`.
   --  ------------------------------
   procedure Run_Migration (Session : in ADO.Sessions.Master_Session'Class;
                            Upgrade : in Upgrade_Type;
                            Files   : in out Util.Strings.Vectors.Vector;
                            Execute : not null
                              access procedure (Session : in ADO.Sessions.Session'Class;
                                                SQL : in String)) is
      function Need_Prepare return Boolean;
      procedure Process (SQL : in String);

      function Need_Prepare return Boolean is
      begin
         if Files.Is_Empty then
            return True;
         end if;
         declare
            Path : constant String := Files.Element (1);
            Dir  : constant String := Ada.Directories.Containing_Directory (Path);
         begin
            return Dir /= To_String (Upgrade.Path);
         end;
      end Need_Prepare;

      procedure Process (SQL : in String) is
      begin
         Execute (Session, SQL);
      end Process;
   begin
      if Need_Prepare then
         Prepare_Migration (Session, Upgrade, Files);
      end if;

      for File of Files loop
         ADO.SQL.Read_File (File, Process'Access);
      end loop;

      declare
         Stmt : ADO.Statements.Update_Statement
            := Session.Create_Statement (ADO.Model.VERSION_TABLE'Access);
         Result : Integer;
      begin
         Stmt.Save_Field ("version", Upgrade.Version);
         Stmt.Set_Filter ("name = :name AND version < :version");
         Stmt.Bind_Param ("name", Upgrade.Name);
         Stmt.Bind_Param ("version", Upgrade.Version);
         Stmt.Execute (Result);
         if Result > 0 then
            Log.Info ("Database module {0} upgraded to version{1}",
                      Upgrade.Name, Upgrade.Version'Image);
            return;
         end if;
      end;

      declare
         Stmt : ADO.Statements.Insert_Statement
            := Session.Create_Statement (ADO.Model.VERSION_TABLE'Access);
         Result : Integer;
      begin
         Stmt.Save_Field ("name", Upgrade.Name);
         Stmt.Save_Field ("version", Upgrade.Version);
         Stmt.Execute (Result);

         Log.Info ("Database module {0} created with version{1}",
                   Upgrade.Name, Upgrade.Version'Image);
      end;
   end Run_Migration;

end ADO.Schemas.Databases;
