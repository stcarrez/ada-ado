-----------------------------------------------------------------------
--  ADO Drivers -- Database Drivers
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Util.Properties;

--  The <b>ADO.Drivers</b> package represents the database driver that will create
--  database connections and provide the database specific implementation.
package ADO.Drivers is

   use Ada.Strings.Unbounded;

   --  Raised when the connection URI is invalid.
   Connection_Error : exception;

   --  Raised for all errors reported by the database
   DB_Error : exception;

   type Driver_Index is new Natural range 0 .. 4;

   --  Initialize the drivers and the library by reading the property file
   --  and configure the runtime with it.
   procedure Initialize (Config : in String);

   --  Initialize the drivers and the library and configure the runtime with the given properties.
   procedure Initialize (Config : in Util.Properties.Manager'Class);

   --  Initialize the drivers which are available.
   procedure Initialize;

   --  Get the global configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Name    : in String;
                        Default : in String := "") return String;

end ADO.Drivers;
