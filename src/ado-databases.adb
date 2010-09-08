-----------------------------------------------------------------------
--  ADO Databases -- Database Connections
--  Copyright (C) 2010 Stephane Carrez
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

with Util.Log;
with Util.Log.Loggers;

with System.Address_Image;
package body ADO.Databases is

   use Util.Log;
   use ADO.Drivers;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Databases");

   --  ------------------------------
   --  Get the database connection status.
   --  ------------------------------
   function Get_Status (Database : in Connection) return Connection_Status is
   begin
      if Database.Impl = null then
         return CLOSED;
      else
         return OPEN;
      end if;
   end Get_Status;

   --  ------------------------------
   --  Create a query statement.  The statement is not prepared
   --  ------------------------------
   function Create_Statement (Database : in Connection;
                              Query    : in String) return Query_Statement'Class is
   begin
      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      return Database.Impl.Create_Statement (Query);
   end Create_Statement;

   function Prepare_Statement (Database : in Connection;
                               Query    : in String) return Query_Statement'Class is
   begin
      if Database.Impl = null then
         Log.Error ("Database connection is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      return Database.Impl.Create_Statement (Query);
   end Prepare_Statement;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   procedure Load_Schema (Database : in Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      if Database.Impl = null then
         Log.Error ("Database connection is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      Database.Impl.Load_Schema (Schema);
   end Load_Schema;

   --  ------------------------------
   --  Close the database connection
   --  ------------------------------
   procedure Close (Database : in out Connection) is
   begin
      Log.Info ("Closing database connection");

      if Database.Impl /= null then
         Database.Impl.Close;
      end if;
   end Close;

   --  ------------------------------
   --  Start a transaction.
   --  ------------------------------
   procedure Begin_Transaction (Database : in out Master_Connection) is
   begin
      Log.Info ("Begin transaction");

      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      Database.Impl.Begin_Transaction;
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   procedure Commit (Database : in out Master_Connection) is
   begin
      Log.Info ("Commit transaction");

      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      Database.Impl.Commit;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   procedure Rollback (Database : in out Master_Connection) is
   begin
      Log.Info ("Rollback transaction");

      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "Database implementation is not initialized";
      end if;
      Database.Impl.Rollback;
   end Rollback;

   procedure Delete (Database : in Master_Connection;
                     Name     : in String;
                     Id       : in Identifier) is
--      Sql : Query_String;
   begin
--      Append (Source => Sql, New_Item => "DELETE from '");
--      Append_Escape (SQL => Sql, Value => Name);
--      Append (Source => Sql, New_Item => "' WHERE id = ");
--      Append (SQL => Sql, Value => Id);
--      Database.Execute (Sql);
      null;
   end Delete;

   procedure Execute (Database : in Master_Connection;
                      SQL      : in Query_String) is
   begin
      Log.Info ("Execute: {0}", SQL);

      Database.Impl.Execute (SQL);
   end Execute;

   procedure Execute (Database : in Master_Connection;
                      SQL      : in Query_String;
                      Id       : out Identifier) is
   begin
      Log.Info ("Execute: {0}", SQL);

      Database.Impl.Execute (SQL, Id);
   end Execute;

   --  ------------------------------
   --  Adjust the connection reference counter
   --  ------------------------------
   overriding
   procedure Adjust   (Object : in out Connection) is
   begin
      if Object.Impl /= null then
         Log.Info ("Adjust {0} : {1}", System.Address_Image (Object.Impl.all'Address),
                   Natural'Image (Object.Impl.Count));
         Object.Impl.Count := Object.Impl.Count + 1;
      end if;
   end Adjust;

   --  ------------------------------
   --  Releases the connection reference counter
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Connection) is
   begin
      if Object.Impl /= null then
         Log.Info ("Finalize {0} : {1}", System.Address_Image (Object.Impl.all'Address),
                  Natural'Image (Object.Impl.Count));
         Object.Impl.Count := Object.Impl.Count - 1;
         if Object.Impl.Count = 0 then
            Release (Object.Impl);
            Object.Impl := null;
         end if;
      end if;
   end Finalize;

   --  The default data source.
   DS : DataSource_Access := null;

   --  ------------------------------
   --  Get the default data source
   --  ------------------------------
   function Get_DataSource return DataSource'Class is
   begin
      return DS.all;
   end Get_DataSource;

   --  ------------------------------
   --  Set the default data source
   --  ------------------------------
   procedure Set_DataSource (Controller : in DataSource_Access) is
   begin
      DS := Controller;
   end Set_DataSource;

   --  ------------------------------
   --  Get a read-only database connection from the default data source.
   --  ------------------------------
   function Get_Connection return Master_Connection'Class is
   begin
      Log.Info ("Get master connection");
      if DS = null then
         raise NOT_OPEN with "No default data source";
      end if;
      return DS.Get_Connection;
   end Get_Connection;


   --  ------------------------------
   --  Attempts to establish a connection with the data source
   --  that this DataSource object represents.
   --  ------------------------------
   function Get_Connection (Controller : in DataSource)
                           return Master_Connection'Class is
      Connection : ADO.Drivers.Database_Connection_Access;
   begin
      Log.Info ("Get master connection from data-source");

      Controller.Create_Connection (Connection);
      return Master_Connection '(Ada.Finalization.Controlled with
                                 Impl => Connection);
   end Get_Connection;

   --  ------------------------------
   --  Set the master data source
   --  ------------------------------
   procedure Set_Master (Controller : in out Replicated_DataSource;
                         Master     : in DataSource_Access) is
   begin
      Controller.Master := Master;
   end Set_Master;

   --  ------------------------------
   --  Get the master data source
   --  ------------------------------
   function Get_Master (Controller : in Replicated_DataSource)
                       return DataSource_Access is
   begin
      return Controller.Master;
   end Get_Master;

   --  ------------------------------
   --  Set the slave data source
   --  ------------------------------
   procedure Set_Slave (Controller : in out Replicated_DataSource;
                        Slave      : in DataSource_Access) is
   begin
      Controller.Slave := Slave;
   end Set_Slave;

   --  ------------------------------
   --  Get the slave data source
   --  ------------------------------
   function Get_Slave (Controller : in Replicated_DataSource)
                      return DataSource_Access is
   begin
      return Controller.Slave;
   end Get_Slave;

   --  ------------------------------
   --  Get a slave database connection
   --  ------------------------------
   function Get_Slave_Connection (Controller : in Replicated_DataSource)
                                 return Connection'Class is
   begin
      return Controller.Slave.Get_Connection;
   end Get_Slave_Connection;

end ADO.Databases;
