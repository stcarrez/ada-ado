-----------------------------------------------------------------------
--  ado-queries-loaders -- Loader for Database Queries
--  Copyright (C) 2011, 2012, 2017, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Connections;
package ADO.Queries.Loaders is

   generic
      Path : String;
      Sha1 : String;
   package File is
      Name : aliased constant String := Path;
      Hash : aliased constant String := Sha1;
      File : aliased Query_File;
   end File;

   generic
      Name : String;
      File : Query_File_Access;
   package Query is
      Query_Name : aliased constant String := Name;
      Query      : aliased Query_Definition;
   end Query;

   --  Read the query definition.
   procedure Read_Query (Manager : in Query_Manager;
                         Into    : in Query_Definition_Access);

   --  Register the query definition in the query file.  Registration is done
   --  in the package elaboration phase.
   procedure Register (File  : in Query_File_Access;
                       Query : in Query_Definition_Access);

   --  Initialize the queries to look in the list of directories specified by <b>Paths</b>.
   --  Each search directory is separated by ';' (yes, even on Unix).
   --  When <b>Load</b> is true, read the XML query file and initialize the query
   --  definitions from that file.
   procedure Initialize (Manager : in out Query_Manager;
                         Config  : in ADO.Connections.Configuration'Class);

   --  Find the query identified by the given name.
   function Find_Query (Name : in String) return Query_Definition_Access;

private

   --  Returns True if the XML query file must be reloaded.
   function Is_Modified (File : in out Query_File_Info) return Boolean;

   --  Read the query file and all the associated definitions.
   procedure Read_Query (Manager : in Query_Manager;
                         File    : in out Query_File_Info);

end ADO.Queries.Loaders;
