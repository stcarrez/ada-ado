-----------------------------------------------------------------------
--  ado-queries-loaders -- Loader for Database Queries
--  Copyright (C) 2011, 2012, 2013, 2014, 2017, 2018, 2019, 2020 Stephane Carrez
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

with Interfaces;

with Ada.IO_Exceptions;
with Ada.Directories;

with Util.Files;
with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
with ADO.Configs;
package body ADO.Queries.Loaders is

   use Util.Log;
   use ADO.Connections;
   use Interfaces;
   use Ada.Calendar;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Queries.Loaders");

   Base_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year  => 1970,
                                                                   Month => 1,
                                                                   Day   => 1);

   --  Check for file modification time at most every 60 seconds.
   FILE_CHECK_DELTA_TIME : constant Unsigned_32 := 60;

   --  The list of query files defined by the application.
   Query_Files : Query_File_Access := null;

   Last_Query  : Query_Index := 0;
   Last_File   : File_Index := 0;

   --  Convert a Time to an Unsigned_32.
   function To_Unsigned_32 (T : in Ada.Calendar.Time) return Unsigned_32;
   pragma Inline_Always (To_Unsigned_32);

   --  Get the modification time of the XML query file associated with the query.
   function Modification_Time (File : in Query_File_Info) return Unsigned_32;

   --  Initialize the query SQL pattern with the value
   procedure Set_Query_Pattern (Into  : in out Query_Pattern;
                                Value : in Util.Beans.Objects.Object);

   --  ------------------------------
   --  Register the query definition in the query file.  Registration is done
   --  in the package elaboration phase.
   --  ------------------------------
   procedure Register (File  : in Query_File_Access;
                       Query : in Query_Definition_Access) is
   begin
      Last_Query   := Last_Query + 1;
      Query.File   := File;
      Query.Next   := File.Queries;
      Query.Query  := Last_Query;
      File.Queries := Query;
      if File.Next = null and then Query_Files /= File then
         Last_File := Last_File + 1;
         File.Next := Query_Files;
         File.File := Last_File;
         Query_Files := File;
      end if;
   end Register;

   function Find_Driver (Name : in String) return Integer;

   function Find_Driver (Name : in String) return Integer is
   begin
      if Name'Length = 0 then
         return 0;
      end if;
      declare
         Driver : constant Connections.Driver_Access
           := Connections.Get_Driver (Name);
      begin
         if Driver = null then
            --  There is no problem to have an SQL query for unsupported drivers, but still
            --  report some warning.
            Log.Warn ("Database driver {0} not found", Name);
            return -1;
         end if;
         return Integer (Driver.Get_Driver_Index);
      end;
   end Find_Driver;

   --  ------------------------------
   --  Convert a Time to an Unsigned_32.
   --  ------------------------------
   function To_Unsigned_32 (T : in Ada.Calendar.Time) return Unsigned_32 is
      D : constant Duration := Duration '(T - Base_Time);
   begin
      return Unsigned_32 (D);
   end To_Unsigned_32;

   --  ------------------------------
   --  Get the modification time of the XML query file associated with the query.
   --  ------------------------------
   function Modification_Time (File : in Query_File_Info) return Unsigned_32 is
      Path : constant String := To_String (File.Path);
   begin
      return To_Unsigned_32 (Ada.Directories.Modification_Time (Path));

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("XML query file '{0}' does not exist", Path);
         return 0;
   end Modification_Time;

   --  ------------------------------
   --  Returns True if the XML query file must be reloaded.
   --  ------------------------------
   function Is_Modified (File : in out Query_File_Info) return Boolean is
      Now : constant Unsigned_32 := To_Unsigned_32 (Ada.Calendar.Clock);
   begin
      --  Have we passed the next check time?
      if File.Next_Check > Now then
         return False;
      end if;

      --  Next check in N seconds (60 by default).
      File.Next_Check := Now + FILE_CHECK_DELTA_TIME;

      --  See if the file was changed.
      declare
         M : constant Unsigned_32 := Modification_Time (File);
      begin
         if File.Last_Modified = M then
            return False;
         end if;
         File.Last_Modified := M;
         return True;
      end;
   end Is_Modified;

   --  ------------------------------
   --  Initialize the query SQL pattern with the value
   --  ------------------------------
   procedure Set_Query_Pattern (Into  : in out Query_Pattern;
                                Value : in Util.Beans.Objects.Object) is
   begin
      Into.SQL := Util.Beans.Objects.To_Unbounded_String (Value);
   end Set_Query_Pattern;

   procedure Read_Query (Manager : in Query_Manager;
                         File    : in out Query_File_Info) is

      type Query_Info_Fields is (FIELD_CLASS_NAME, FIELD_PROPERTY_TYPE,
                                 FIELD_PROPERTY_NAME, FIELD_QUERY_NAME,
                                 FIELD_SQL_DRIVER,
                                 FIELD_SQL, FIELD_SQL_COUNT, FIELD_QUERY);

      --  The Query_Loader holds context and state information for loading
      --  the XML query file and initializing the Query_Definition.
      type Query_Loader is record
         --  File       : Query_File_Access;
         Hash_Value : Unbounded_String;
         Query_Def  : Query_Definition_Access;
         Query      : Query_Info_Ref.Ref;
         Driver     : Integer;
      end record;
      type Query_Loader_Access is access all Query_Loader;

      procedure Set_Member (Into  : in out Query_Loader;
                            Field : in Query_Info_Fields;
                            Value : in Util.Beans.Objects.Object);

      --  ------------------------------
      --  Called by the de-serialization when a given field is recognized.
      --  ------------------------------
      procedure Set_Member (Into  : in out Query_Loader;
                            Field : in Query_Info_Fields;
                            Value : in Util.Beans.Objects.Object) is
      begin
         case Field is
         when FIELD_CLASS_NAME =>
            Append (Into.Hash_Value, " class=");
            Append (Into.Hash_Value, Util.Beans.Objects.To_Unbounded_String (Value));

         when FIELD_PROPERTY_TYPE =>
            Append (Into.Hash_Value, " type=");
            Append (Into.Hash_Value, Util.Beans.Objects.To_Unbounded_String (Value));

         when FIELD_PROPERTY_NAME =>
            Append (Into.Hash_Value, " name=");
            Append (Into.Hash_Value, Util.Beans.Objects.To_Unbounded_String (Value));

         when FIELD_QUERY_NAME =>
            Into.Query_Def  := Find_Query (File, Util.Beans.Objects.To_String (Value));
            Into.Driver := 0;
            if Into.Query_Def /= null then
               Into.Query := Query_Info_Ref.Create;
            end if;

         when FIELD_SQL_DRIVER =>
            Into.Driver := Find_Driver (Util.Beans.Objects.To_String (Value));

         when FIELD_SQL =>
            if not Into.Query.Is_Null and Into.Driver >= 0 and Into.Query_Def /= null then
               Set_Query_Pattern (Into.Query.Value.Main_Query (Driver_Index (Into.Driver)),
                                  Value);
            end if;
            Into.Driver := 0;

         when FIELD_SQL_COUNT =>
            if not Into.Query.Is_Null and Into.Driver >= 0 and Into.Query_Def /= null then
               Set_Query_Pattern (Into.Query.Value.Count_Query (Driver_Index (Into.Driver)),
                                  Value);
            end if;
            Into.Driver := 0;

         when FIELD_QUERY =>
            if Into.Query_Def /= null then
               --  Now we can safely setup the query info associated with the query definition.
               Manager.Queries (Into.Query_Def.Query) := Into.Query;
            end if;
            Into.Query_Def := null;

         end case;
      end Set_Member;

      package Query_Mapper is
        new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Query_Loader,
                                                  Element_Type_Access => Query_Loader_Access,
                                                  Fields              => Query_Info_Fields,
                                                  Set_Member          => Set_Member);

      Loader     : aliased Query_Loader;
      Sql_Mapper : aliased Query_Mapper.Mapper;
      Reader     : Util.Serialize.IO.XML.Parser;
      Mapper     : Util.Serialize.Mappers.Processing;
      Path       : constant String := To_String (File.Path);
   begin
      Log.Info ("Reading XML query {0}", Path);
      --  Loader.File   := Into;
      Loader.Driver := 0;

      --  Create the mapping to load the XML query file.
      Sql_Mapper.Add_Mapping ("class/@name", FIELD_CLASS_NAME);
      Sql_Mapper.Add_Mapping ("class/property/@type", FIELD_PROPERTY_TYPE);
      Sql_Mapper.Add_Mapping ("class/property/@name", FIELD_PROPERTY_NAME);
      Sql_Mapper.Add_Mapping ("query/@name", FIELD_QUERY_NAME);
      Sql_Mapper.Add_Mapping ("query/sql", FIELD_SQL);
      Sql_Mapper.Add_Mapping ("query/sql/@driver", FIELD_SQL_DRIVER);
      Sql_Mapper.Add_Mapping ("query/sql-count", FIELD_SQL_COUNT);
      Sql_Mapper.Add_Mapping ("query/sql-count/@driver", FIELD_SQL_DRIVER);
      Sql_Mapper.Add_Mapping ("query", FIELD_QUERY);
      Mapper.Add_Mapping ("query-mapping", Sql_Mapper'Unchecked_Access);

      --  Set the context for Set_Member.
      Query_Mapper.Set_Context (Mapper, Loader'Access);

      --  Read the XML query file.
      Reader.Parse (Path, Mapper);

      File.Next_Check := To_Unsigned_32 (Ada.Calendar.Clock) + FILE_CHECK_DELTA_TIME;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("XML query file '{0}' does not exist", Path);

   end Read_Query;

   --  ------------------------------
   --  Read the query definition.
   --  ------------------------------
   procedure Read_Query (Manager : in Query_Manager;
                         Into    : in Query_Definition_Access) is
   begin
      if Manager.Queries (Into.Query).Is_Null
        or else Is_Modified (Manager.Files (Into.File.File))
      then
         Read_Query (Manager, Manager.Files (Into.File.File));
      end if;
   end Read_Query;

   --  ------------------------------
   --  Initialize the queries to look in the list of directories specified by <b>Paths</b>.
   --  Each search directory is separated by ';' (yes, even on Unix).
   --  When <b>Load</b> is true, read the XML query file and initialize the query
   --  definitions from that file.
   --  ------------------------------
   procedure Initialize (Manager : in out Query_Manager;
                         Config  : in ADO.Connections.Configuration'Class) is
      function Get_Config (Name : in String) return String;

      function Get_Config (Name : in String) return String is
         Value : constant String := Config.Get_Property (Name);
      begin
         if Value'Length > 0 then
            return Value;
         else
            return ADO.Configs.Get_Config (Name);
         end if;
      end Get_Config;

      Paths : constant String := Get_Config (Configs.QUERY_PATHS_CONFIG);
      Load  : constant Boolean := Get_Config (Configs.QUERY_LOAD_CONFIG) = "true";
      File  : Query_File_Access := Query_Files;
   begin
      Log.Info ("Initializing query search paths to {0}", Paths);

      if Manager.Queries = null then
         Manager.Queries := new Query_Table (1 .. Last_Query);
      end if;
      if Manager.Files = null then
         Manager.Files := new File_Table (1 .. Last_File);
      end if;
      Manager.Driver := Config.Get_Driver;
      while File /= null loop
         declare
            Path : constant String := Util.Files.Find_File_Path (Name  => File.Name.all,
                                                                 Paths => Paths);
         begin
            Manager.Files (File.File).File := File;
            Manager.Files (File.File).Last_Modified := 0;
            Manager.Files (File.File).Next_Check := 0;
            Manager.Files (File.File).Path := To_Unbounded_String (Path);

            if Load then
               Read_Query (Manager, Manager.Files (File.File));
            end if;
         end;
         File := File.Next;
      end loop;
   end Initialize;

   --  ------------------------------
   --  Find the query identified by the given name.
   --  ------------------------------
   function Find_Query (Name : in String) return Query_Definition_Access is
      File : Query_File_Access := Query_Files;
   begin
      while File /= null loop
         declare
            Query : Query_Definition_Access := File.Queries;
         begin
            while Query /= null loop
               if Query.Name.all = Name then
                  return Query;
               end if;
               Query := Query.Next;
            end loop;
         end;
         File := File.Next;
      end loop;
      Log.Warn ("Query {0} not found", Name);
      return null;
   end Find_Query;

   package body File is
   begin
      File.Name := Name'Access;
      File.Sha1_Map := Hash'Access;
   end File;

   package body Query is
   begin
      Query.Name := Query_Name'Access;
      Query.File := File;
      Register (File  => File, Query => Query'Access);
   end Query;

end ADO.Queries.Loaders;
