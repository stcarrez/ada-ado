-----------------------------------------------------------------------
--  ADO Mysql Database -- MySQL Database connections
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Mysql.Mysql; use Mysql.Mysql;

package ADO.Drivers.Mysql is

   type Mysql_Driver is limited private;

   --  Initialize the Mysql driver.
   procedure Initialize;

private

   --  Create a new MySQL connection using the configuration parameters.
   procedure Create_Connection (D      : in out Mysql_Driver;
                                Config : in Configuration'Class;
                                Result : out Database_Connection_Access);

   type Mysql_Driver is new ADO.Drivers.Driver with null record;

   --  Deletes the Mysql driver.
   overriding
   procedure Finalize (D : in out Mysql_Driver);

   --  Database connection implementation
   type Database_Connection is new ADO.Drivers.Database_Connection with record
      Name        : Unbounded_String := Null_Unbounded_String;
      Server_Name : Unbounded_String := Null_Unbounded_String;
      Login_Name  : Unbounded_String := Null_Unbounded_String;
      Password    : Unbounded_String := Null_Unbounded_String;

      Connected   : Boolean      := False;
      Server      : Mysql_Access := null;

      --  MySQL autocommit flag
      Autocommit  : Boolean := True;
   end record;

   type Database_Connection_Access is access all Database_Connection'Class;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement_Access;

   --  Create a delete statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access;

   --  Create an insert statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access;

   --  Create an update statement.
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access;

   --  Start a transaction.
   overriding
   procedure Begin_Transaction (Database : in out Database_Connection);

   --  Commit the current transaction.
   overriding
   procedure Commit (Database : in out Database_Connection);

   --  Rollback the current transaction.
   overriding
   procedure Rollback (Database : in out Database_Connection);

   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String);

   --  Closes the database connection
   overriding
   procedure Close (Database : in out Database_Connection);

   overriding
   procedure Finalize (Database : in out Database_Connection);

   --  Load the database schema definition for the current database.
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition);

end ADO.Drivers.Mysql;
