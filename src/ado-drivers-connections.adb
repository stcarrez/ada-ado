-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Systems.DLLs;

with System;
with Ada.Strings.Fixed;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;

package body ADO.Drivers.Connections is

   use Ada.Strings.Fixed;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Drivers.Connections");

   --  Load the database driver.
   procedure Load_Driver (Name : in String);

   --  ------------------------------
   --  Set the connection URL to connect to the database.
   --  The driver connection is a string of the form:
   --
   --   driver://[host][:port]/[database][?property1][=value1]...
   --
   --  If the string is invalid or if the driver cannot be found,
   --  the Connection_Error exception is raised.
   --  ------------------------------
   procedure Set_Connection (Controller : in out Configuration;
                             URI        : in String) is

      Pos, Pos2, Slash_Pos, Next : Natural;
   begin
      Log.Info ("Set connection URI: {0}", URI);

      Controller.URI := To_Unbounded_String (URI);
      Pos := Index (URI, "://");
      if Pos <= 1 then
         Log.Error ("Invalid connection URI: {0}", URI);
         raise Connection_Error
           with "Invalid URI: '" & URI & "'";
      end if;
      Controller.Driver := Get_Driver (URI (URI'First .. Pos - 1));
      if Controller.Driver = null then
         Log.Error ("No driver found for connection URI: {0}", URI);
         raise Connection_Error
           with "Driver '" & URI (URI'First .. Pos - 1) & "' not found";
      end if;

      Pos := Pos + 3;
      Slash_Pos := Index (URI, "/", Pos);
      if Slash_Pos < Pos then
         Log.Error ("Invalid connection URI: {0}", URI);
         raise Connection_Error
           with "Invalid connection URI: '" & URI & "'";
      end if;

      --  Extract the server and port.
      Pos2 := Index (URI, ":", Pos);
      if Pos2 >= Pos then
         Controller.Server := To_Unbounded_String (URI (Pos .. Pos2 - 1));
         begin
            Controller.Port := Natural'Value (URI (Pos2 + 1 .. Slash_Pos - 1));

         exception
            when Constraint_Error =>
               Log.Error ("Invalid port in connection URI: {0}", URI);
               raise Connection_Error
                 with "Invalid port in connection URI: '" & URI & "'";
         end;
      else
         Controller.Port := 0;
         Controller.Server := To_Unbounded_String (URI (Pos .. Slash_Pos - 1));
      end if;

      --  Extract the database name.
      Pos := Index (URI, "?", Slash_Pos);
      if Pos - 1 > Slash_Pos + 1 then
         Controller.Database := To_Unbounded_String (URI (Slash_Pos + 1 .. Pos - 1));
      elsif Pos = 0 and Slash_Pos + 1 < URI'Last then
         Controller.Database := To_Unbounded_String (URI (Slash_Pos + 1 .. URI'Last));
      else
         Controller.Database := Null_Unbounded_String;
      end if;

      --  Parse the optional properties
      if Pos > Slash_Pos then
         while Pos < URI'Last loop
            Pos2 := Index (URI, "=", Pos + 1);
            if Pos2 > Pos then
               Next := Index (URI, "&", Pos2 + 1);
               if Next > 0 then
                  Controller.Properties.Set (URI (Pos + 1 .. Pos2 - 1),
                                             URI (Pos2 + 1 .. Next - 1));
                  Pos := Next;
               else
                  Controller.Properties.Set (URI (Pos + 1 .. Pos2 - 1),
                                             URI (Pos2 + 1 .. URI'Last));
                  Pos := URI'Last;
               end if;
            else
               Controller.Properties.Set (URI (Pos + 1 .. URI'Last), "");
               Pos := URI'Last;
            end if;
         end loop;
      end if;
   end Set_Connection;

   --  ------------------------------
   --  Set a property on the datasource for the driver.
   --  The driver can retrieve the property to configure and open
   --  the database connection.
   --  ------------------------------
   procedure Set_Property (Controller : in out Configuration;
                           Name       : in String;
                           Value      : in String) is
   begin
      Controller.Properties.Set (Name, Value);
   end Set_Property;

   --  ------------------------------
   --  Get a property from the datasource configuration.
   --  If the property does not exist, an empty string is returned.
   --  ------------------------------
   function Get_Property (Controller : in Configuration;
                          Name       : in String) return String is
   begin
      return Controller.Properties.Get (Name, "");
   end Get_Property;

   --  ------------------------------
   --  Get the server hostname.
   --  ------------------------------
   function Get_Server (Controller : in Configuration) return String is
   begin
      return To_String (Controller.Server);
   end Get_Server;

   --  ------------------------------
   --  Set the server hostname.
   --  ------------------------------
   procedure Set_Server (Controller : in out Configuration;
                         Server     : in String) is
   begin
      Controller.Server := To_Unbounded_String (Server);
   end Set_Server;

   --  ------------------------------
   --  Set the server port.
   --  ------------------------------
   procedure Set_Port (Controller : in out Configuration;
                       Port       : in Natural) is
   begin
      Controller.Port := Port;
   end Set_Port;

   --  ------------------------------
   --  Get the server port.
   --  ------------------------------
   function Get_Port (Controller : in Configuration) return Natural is
   begin
      return Controller.Port;
   end Get_Port;

   --  ------------------------------
   --  Get the database name.
   --  ------------------------------
   function Get_Database (Controller : in Configuration) return String is
   begin
      return To_String (Controller.Database);
   end Get_Database;

   --  ------------------------------
   --  Create a new connection using the configuration parameters.
   --  ------------------------------
   procedure Create_Connection (Config : in Configuration'Class;
                                Result : out Database_Connection_Access) is
   begin
      if Config.Driver = null then
         Log.Error ("No driver found for connection {0}", To_String (Config.URI));
         raise Connection_Error with "Data source is not initialized: driver not found";
      end if;
      Config.Driver.Create_Connection (Config, Result);
      Log.Info ("Created connection to '{0}' -> {1}", Config.URI, Result.Ident);

   exception
      when others =>
         Log.Info ("Failed to create connection to '{0}'", Config.URI);
         raise;

   end Create_Connection;

   package Driver_List is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Driver_Access);

   Next_Index : Driver_Index := 1;
   Drivers    : Driver_List.List;

   --  ------------------------------
   --  Get the driver unique index.
   --  ------------------------------
   function Get_Driver_Index (D : in Driver) return Driver_Index is
   begin
      return D.Index;
   end Get_Driver_Index;

   --  ------------------------------
   --  Get the driver name.
   --  ------------------------------
   function Get_Driver_Name (D : in Driver) return String is
   begin
      return D.Name.all;
   end Get_Driver_Name;

   --  ------------------------------
   --  Register a database driver.
   --  ------------------------------
   procedure Register (Driver : in Driver_Access) is
   begin
      Log.Info ("Register driver {0}", Driver.Name.all);

      Driver_List.Prepend (Container => Drivers, New_Item => Driver);
      Driver.Index := Next_Index;
      Next_Index := Next_Index + 1;
   end Register;

   --  ------------------------------
   --  Load the database driver.
   --  ------------------------------
   procedure Load_Driver (Name : in String) is
      Lib    : constant String := "libada_ado_" & Name & Util.Systems.DLLs.Extension;
      Symbol : constant String := "ado__drivers__connections__" & Name & "__initialize";
      Handle : Util.Systems.DLLs.Handle;
      Addr   : System.Address;

   begin
      Log.Info ("Loading driver {0}", Lib);
      Handle := Util.Systems.DLLs.Load (Lib);
      Addr := Util.Systems.DLLs.Get_Symbol (Handle, Symbol);

      declare
         procedure Init;
         pragma Import (C, Init);
         for Init'Address use Addr;
      begin
         Init;
      end;
   exception
      when Util.Systems.DLLs.Not_Found =>
         Log.Error ("Driver for {0} was loaded but does not define the initialization symbol",
                    Name);

      when E : Util.Systems.DLLs.Load_Error =>
         Log.Error ("Driver for {0} was not found: {1}",
                    Name, Ada.Exceptions.Exception_Message (E));

   end Load_Driver;

   --  ------------------------------
   --  Get a database driver given its name.
   --  ------------------------------
   function Get_Driver (Name : in String) return Driver_Access is
   begin
      Log.Info ("Get driver {0}", Name);

      for Retry in 0 .. 1 loop
         if Retry > 0 then
            Load_Driver (Name);
         end if;
         declare
            Iter : Driver_List.Cursor := Driver_List.First (Drivers);
         begin
            while Driver_List.Has_Element (Iter) loop
               declare
                  D : constant Driver_Access := Driver_List.Element (Iter);
               begin
                  if Name = D.Name.all then
                     return D;
                  end if;
               end;
               Driver_List.Next (Iter);
            end loop;
         end;
      end loop;
      return null;
   end Get_Driver;

end ADO.Drivers.Connections;
