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

with ADO.SQL;
with Ada.Unchecked_Deallocation;
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
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement is
   begin
      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      declare
         Query : Query_Statement_Access := Database.Impl.all.Create_Statement (Table);
      begin
         return ADO.Statements.Create_Statement (Query);
      end;
   end Create_Statement;

   --  Create a query statement.  The statement is not prepared
   function Create_Statement (Database : in Connection;
                              Query    : in String)
                              return Query_Statement is
   begin
      if Database.Impl = null then
         Log.Error ("Database implementation is not initialized");
         raise NOT_OPEN with "No connection to the database";
      end if;
      declare
         Stmt : Query_Statement_Access := Database.Impl.all.Create_Statement (null);
      begin
         Append (Query => Stmt.all, SQL => Query);
         return ADO.Statements.Create_Statement (Stmt);
      end;
   end Create_Statement;

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
   --  Create a delete statement.
   --  ------------------------------
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement is
   begin
      Log.Info ("Create delete statement");

      declare
         Stmt : Delete_Statement_Access := Database.Impl.all.Create_Statement (Table);
      begin
         return Create_Statement (Stmt);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement is
   begin
      Log.Info ("Create insert statement");

      return Create_Statement (Database.Impl.all.Create_Statement (Table));
   end Create_Statement;

   --  ------------------------------
   --  Create an update statement.
   --  ------------------------------
   function Create_Statement (Database : in Master_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement is
   begin
      Log.Info ("Create insert statement");

      return Create_Statement (Database.Impl.all.Create_Statement (Table));
   end Create_Statement;

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

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Database_Connection'Class,
                                    Name   => Database_Connection_Access);

   begin
      if Object.Impl /= null then
         Log.Info ("Finalize {0} : {1}", System.Address_Image (Object.Impl.all'Address),
                  Natural'Image (Object.Impl.Count));
         Object.Impl.Count := Object.Impl.Count - 1;
         if Object.Impl.Count = 0 then
            Free (Object.Impl);
         end if;
      end if;
   end Finalize;

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
