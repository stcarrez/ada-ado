-----------------------------------------------------------------------
--  ado-configs -- Database connection configuration
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2022 Stephane Carrez
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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;

package body ADO.Configs is

   use Ada.Strings.Fixed;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Configs");

   --  Global configuration properties (loaded by Initialize).
   Global_Config : Util.Properties.Manager;

   --  ------------------------------
   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   --  ------------------------------
   procedure Initialize (Config : in String) is
   begin
      Log.Info ("Initialize using property file {0}", Config);

      begin
         Util.Properties.Load_Properties (Global_Config, Config);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Error ("Configuration file '{0}' does not exist", Config);
      end;

      Initialize (Global_Config);
   end Initialize;

   --  ------------------------------
   --  Initialize the drivers and the library and configure the runtime with the given properties.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager'Class) is
   begin
      Global_Config := Util.Properties.Manager (Config);
   end Initialize;

   --  ------------------------------
   --  Get the global configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Name    : in String;
                        Default : in String := "") return String is
   begin
      return Global_Config.Get (Name, Default);
   end Get_Config;

   --  ------------------------------
   --  Returns true if the global configuration property is set to true/on.
   --  ------------------------------
   function Is_On (Name   : in String) return Boolean is
      Value : constant String := Global_Config.Get (Name, "");
   begin
      return Value = "on" or else Value = "true" or else Value = "1";
   end Is_On;

   --  ------------------------------
   --  Set the connection URL to connect to the database.
   --  The driver connection is a string of the form:
   --
   --   driver://[host][:port]/[database][?property1][=value1]...
   --
   --  If the string is invalid or if the driver cannot be found,
   --  the Connection_Error exception is raised.
   --  ------------------------------
   procedure Set_Connection (Config : in out Configuration;
                             URI    : in String) is
      Pos, Pos2, Slash_Pos, Next : Natural;
      Is_Hidden : Boolean;
   begin
      Config.URI := To_Unbounded_String (URI);
      Pos := Index (URI, "://");
      if Pos <= URI'First then
         Log.Error ("Invalid connection URI: {0}", URI);
         raise Connection_Error
           with "Invalid URI: '" & URI & "'";
      end if;
      Config.Driver := To_Unbounded_String (URI (URI'First .. Pos - 1));

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
         Config.Server := To_Unbounded_String (URI (Pos .. Pos2 - 1));
         begin
            Config.Port := Natural'Value (URI (Pos2 + 1 .. Slash_Pos - 1));

         exception
            when Constraint_Error =>
               Log.Error ("Invalid port in connection URI: {0}", URI);
               raise Connection_Error
                 with "Invalid port in connection URI: '" & URI & "'";
         end;
      else
         Config.Port := 0;
         Config.Server := To_Unbounded_String (URI (Pos .. Slash_Pos - 1));
      end if;

      --  Extract the database name.
      Pos := Index (URI, "?", Slash_Pos);
      if Pos - 1 > Slash_Pos + 1 then
         Config.Database := To_Unbounded_String (URI (Slash_Pos + 1 .. Pos - 1));
      elsif Pos = 0 and then Slash_Pos + 1 < URI'Last then
         Config.Database := To_Unbounded_String (URI (Slash_Pos + 1 .. URI'Last));
      else
         Config.Database := Null_Unbounded_String;
      end if;

      --  Parse the optional properties
      if Pos > Slash_Pos then
         Config.Log_URI := To_Unbounded_String (URI (URI'First .. Pos));
         while Pos < URI'Last loop
            Pos2 := Index (URI, "=", Pos + 1);
            if Pos2 > Pos then
               Next := Index (URI, "&", Pos2 + 1);
               Append (Config.Log_URI, URI (Pos + 1 .. Pos2));
               Is_Hidden := URI (Pos + 1 .. Pos2 - 1) = "password";
               if Is_Hidden then
                  Append (Config.Log_URI, "XXXXXXX");
               end if;
               if Next > 0 then
                  Config.Properties.Set (URI (Pos + 1 .. Pos2 - 1),
                                         URI (Pos2 + 1 .. Next - 1));
                  if not Is_Hidden then
                     Append (Config.Log_URI, URI (Pos2 + 1 .. Next - 1));
                  end if;
                  Append (Config.Log_URI, "&");
                  Pos := Next;
               else
                  Config.Properties.Set (URI (Pos + 1 .. Pos2 - 1),
                                         URI (Pos2 + 1 .. URI'Last));
                  if not Is_Hidden then
                     Append (Config.Log_URI, URI (Pos2 + 1 .. URI'Last));
                  end if;
                  Pos := URI'Last;
               end if;
            else
               Config.Properties.Set (URI (Pos + 1 .. URI'Last), "");
               Append (Config.Log_URI, URI (Pos + 1 .. URI'Last));
               Pos := URI'Last;
            end if;
         end loop;
      else
         Config.Log_URI := Config.URI;
      end if;
      Log.Info ("Set connection URI: {0}", Config.Log_URI);
   end Set_Connection;

   --  ------------------------------
   --  Get the connection URI that describes the database connection string.
   --  ------------------------------
   function Get_URI (Config : in Configuration) return String is
   begin
      return To_String (Config.URI);
   end Get_URI;

   --  ------------------------------
   --  Get the connection URI that describes the database connection string
   --  but the connection authentication is replaced by XXXX.
   --  ------------------------------
   function Get_Log_URI (Config : in Configuration) return String is
   begin
      return To_String (Config.Log_URI);
   end Get_Log_URI;

   --  ------------------------------
   --  Set a property on the datasource for the driver.
   --  The driver can retrieve the property to configure and open
   --  the database connection.
   --  ------------------------------
   procedure Set_Property (Config : in out Configuration;
                           Name   : in String;
                           Value  : in String) is
   begin
      Config.Properties.Set (Name, Value);
   end Set_Property;

   --  ------------------------------
   --  Get a property from the datasource configuration.
   --  If the property does not exist, an empty string is returned.
   --  ------------------------------
   function Get_Property (Config : in Configuration;
                          Name   : in String) return String is
   begin
      return Config.Properties.Get (Name, "");
   end Get_Property;

   --  ------------------------------
   --  Returns true if the configuration property is set to true/on.
   --  ------------------------------
   function Is_On (Config : in Configuration;
                   Name   : in String) return Boolean is
      Value : constant String := Config.Get_Property (Name);
   begin
      return Value = "on" or else Value = "true" or else Value = "1";
   end Is_On;

   --  ------------------------------
   --  Get the server hostname.
   --  ------------------------------
   function Get_Server (Config : in Configuration) return String is
   begin
      return To_String (Config.Server);
   end Get_Server;

   --  ------------------------------
   --  Set the server hostname.
   --  ------------------------------
   procedure Set_Server (Config : in out Configuration;
                         Server : in String) is
   begin
      Config.Server := To_Unbounded_String (Server);
   end Set_Server;

   --  ------------------------------
   --  Set the server port.
   --  ------------------------------
   procedure Set_Port (Config : in out Configuration;
                       Port   : in Natural) is
   begin
      Config.Port := Port;
   end Set_Port;

   --  ------------------------------
   --  Get the server port.
   --  ------------------------------
   function Get_Port (Config : in Configuration) return Natural is
   begin
      return Config.Port;
   end Get_Port;

   --  ------------------------------
   --  Set the database name.
   --  ------------------------------
   procedure Set_Database (Config   : in out Configuration;
                           Database : in String) is
   begin
      Config.Database := To_Unbounded_String (Database);
   end Set_Database;

   --  ------------------------------
   --  Get the database name.
   --  ------------------------------
   function Get_Database (Config : in Configuration) return String is
   begin
      return To_String (Config.Database);
   end Get_Database;

   --  ------------------------------
   --  Get the database driver name.
   --  ------------------------------
   function Get_Driver (Config : in Configuration) return String is
   begin
      return To_String (Config.Driver);
   end Get_Driver;

   --  ------------------------------
   --  Iterate over the configuration properties and execute the given procedure passing the
   --  property name and its value.
   --  ------------------------------
   procedure Iterate (Config  : in Configuration;
                      Process : access procedure (Name : in String;
                                                  Item : in Util.Properties.Value)) is
   begin
      Config.Properties.Iterate (Process);
   end Iterate;

end ADO.Configs;
