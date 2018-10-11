-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018 Stephane Carrez
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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;

package body ADO.Drivers.Connections is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Drivers.Connections");

   --  Load the database driver.
   procedure Load_Driver (Name : in String);

   --  ------------------------------
   --  Get the driver index that corresponds to the driver for this database connection string.
   --  ------------------------------
   function Get_Driver (Config : in Configuration) return Driver_Index is
      Driver  : constant Driver_Access := Get_Driver (Config.Get_Driver);
   begin
      if Driver = null then
         return Driver_Index'First;
      else
         return Driver.Get_Driver_Index;
      end if;
   end Get_Driver;

   --  ------------------------------
   --  Create a new connection using the configuration parameters.
   --  ------------------------------
   procedure Create_Connection (Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class) is
      Driver  : Driver_Access;
      Log_URI : constant String := Config.Get_Log_URI;
   begin
      Driver := Get_Driver (Config.Get_Driver);
      if Driver = null then
         Log.Error ("No driver found for connection {0}", Log_URI);
         raise ADO.Configs.Connection_Error with
           "Data source is not initialized: driver '" & Config.Get_Driver & "' not found";
      end if;
      Driver.Create_Connection (Config, Result);
      if Result.Is_Null then
         Log.Error ("Driver {0} failed to create connection {0}",
                    Driver.Name.all, Log_URI);
         raise ADO.Configs.Connection_Error with
           "Data source is not initialized: driver error";
      end if;
      Log.Info ("Created connection to '{0}' -> {1}", Log_URI, Result.Value.Ident);

   exception
      when others =>
         Log.Info ("Failed to create connection to '{0}'", Log_URI);
         raise;

   end Create_Connection;

   --  Get the database driver index.
   function Get_Driver_Index (Database : in Database_Connection) return Driver_Index is
      Driver : constant ADO.Drivers.Connections.Driver_Access
         := Database_Connection'Class (Database).Get_Driver;
   begin
      return Driver.Get_Driver_Index;
   end Get_Driver_Index;

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
      Log.Debug ("Loading driver {0}", Lib);
      Handle := Util.Systems.DLLs.Load (Lib);
      Addr := Util.Systems.DLLs.Get_Symbol (Handle, Symbol);

      declare
         procedure Init;
         pragma Import (C, Init);
         for Init'Address use Addr;
      begin
         Log.Info ("Initialising driver {0}", Lib);
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
      Log.Debug ("Get driver {0}", Name);

      if Name'Length = 0 then
         return null;
      end if;

      for Retry in 0 .. 2 loop
         if Retry = 1 then
            ADO.Drivers.Initialize;
         elsif Retry = 2 then
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
