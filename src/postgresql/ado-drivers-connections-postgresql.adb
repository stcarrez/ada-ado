-----------------------------------------------------------------------
--  ADO Postgresql Database -- Postgresql Database connections
--  Copyright (C) 2018 Stephane Carrez
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

with Ada.Task_Identification;

with Interfaces.C.Strings;
with Util.Log;
with Util.Log.Loggers;
with ADO.Statements.Postgresql;
with ADO.Schemas.Postgresql;
with ADO.Sessions;
with ADO.C;
package body ADO.Drivers.Connections.Postgresql is

   use ADO.Statements.Postgresql;
   use Interfaces.C;
   use type PQ.PGconn_Access;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Databases.Postgresql");

   Driver_Name : aliased constant String := "postgresql";
   Driver      : aliased Postgresql_Driver;

   --  ------------------------------
   --  Get the database driver which manages this connection.
   --  ------------------------------
   overriding
   function Get_Driver (Database : in Database_Connection)
                        return ADO.Drivers.Connections.Driver_Access is
      pragma Unreferenced (Database);
   begin
      return Driver'Access;
   end Get_Driver;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   overriding
   function Create_Statement (Database : in Database_Connection;
                              Query    : in String)
                              return Query_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Query => Query);
   end Create_Statement;

   --  ------------------------------
   --  Create a delete statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Create an insert statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Create an update statement.
   --  ------------------------------
   overriding
   function Create_Statement (Database : in Database_Connection;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement_Access is
   begin
      return Create_Statement (Database => Database.Server, Table => Table);
   end Create_Statement;

   --  ------------------------------
   --  Start a transaction.
   --  ------------------------------
   procedure Begin_Transaction (Database : in out Database_Connection) is
   begin
      Database.Execute ("BEGIN");
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   procedure Commit (Database : in out Database_Connection) is
   begin
      Database.Execute ("COMMIT");
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   procedure Rollback (Database : in out Database_Connection) is
   begin
      Database.Execute ("ROLLBACK");
   end Rollback;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   overriding
   procedure Load_Schema (Database : in Database_Connection;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      ADO.Schemas.Postgresql.Load_Schema (Database, Schema,
                                          Ada.Strings.Unbounded.To_String (Database.Name));
   end Load_Schema;

   --  ------------------------------
   --  Execute a simple SQL statement
   --  ------------------------------
   procedure Execute (Database : in out Database_Connection;
                      SQL      : in Query_String) is
      SQL_Stat  : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (SQL);
      Result    : PQ.PGresult_Access;
   begin
      Log.Debug ("Execute SQL: {0}", SQL);
      if Database.Server = PQ.Null_PGconn then
         Log.Error ("Database connection is not open");
         raise ADO.Sessions.Session_Error with "Database connection is closed";
      end if;

      Result := PQ.PQexec (Database.Server, ADO.C.To_C (SQL_Stat));
      Log.Debug ("Query result: {0}", PQ.ExecStatusType'Image (PQ.PQresultStatus (Result)));
      PQ.PQclear (Result);
   end Execute;

   --  ------------------------------
   --  Closes the database connection
   --  ------------------------------
   overriding
   procedure Close (Database : in out Database_Connection) is
   begin
      if Database.Server /= PQ.Null_PGconn then
         Log.Info ("Closing connection {0}/{1}", Database.Name, Database.Ident);

         PQ.PQfinish (Database.Server);
         Database.Server := PQ.Null_PGconn;
      end if;
   end Close;

   --  ------------------------------
   --  Releases the Postgresql connection if it is open
   --  ------------------------------
   overriding
   procedure Finalize (Database : in out Database_Connection) is
   begin
      Log.Debug ("Release connection {0}/{1}", Database.Name, Database.Ident);
      Database.Close;
   end Finalize;

   --  ------------------------------
   --  Initialize the database connection manager.
   --
   --  Postgresql://localhost:3306/db
   --
   --  ------------------------------
   procedure Create_Connection (D      : in out Postgresql_Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class) is

      use type PQ.ConnStatusType;

      URI        : constant ADO.C.String_Ptr := ADO.C.To_String_Ptr (Config.URI);
      Connection : PQ.PGconn_Access;
   begin
      Log.Info ("Task {0} connecting to {1}:{2}",
                Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task),
                To_String (Config.Server), To_String (Config.Database));
      if Config.Get_Property ("password") = "" then
         Log.Debug ("Postgresql connection with user={0}", Config.Get_Property ("user"));
      else
         Log.Debug ("Postgresql connection with user={0} password=XXXXXXXX",
                    Config.Get_Property ("user"));
      end if;
      Connection := PQ.PQconnectdb (ADO.C.To_C (URI));

      if Connection = PQ.Null_PGconn then
         declare
            Message : constant String := "memory allocation error";
         begin
            Log.Error ("Cannot connect to '{0}': {1}", To_String (Config.Log_URI), Message);
            raise Connection_Error with "Cannot connect to Postgresql server: " & Message;
         end;
      end if;

      if PQ.PQstatus (Connection) /= PQ.CONNECTION_OK then
         declare
            Message : constant String := Strings.Value (PQ.PQerrorMessage (Connection));
         begin
            Log.Error ("Cannot connect to '{0}': {1}", To_String (Config.Log_URI), Message);
            raise Connection_Error with "Cannot connect to Postgresql server: " & Message;
         end;
      end if;

      D.Id := D.Id + 1;
      declare
         Ident    : constant String := Util.Strings.Image (D.Id);
         Database : constant Database_Connection_Access := new Database_Connection;
      begin
         Database.Ident (1 .. Ident'Length) := Ident;
         Database.Server := Connection;
         Database.Name   := Config.Database;
         Result := Ref.Create (Database.all'Access);
      end;
   end Create_Connection;

   --  ------------------------------
   --  Initialize the Postgresql driver.
   --  ------------------------------
   procedure Initialize is
      use type Util.Strings.Name_Access;
   begin
      Log.Debug ("Initializing Postgresql driver");

      if Driver.Name = null then
         Driver.Name := Driver_Name'Access;
         Register (Driver'Access);
      end if;
   end Initialize;

   --  ------------------------------
   --  Deletes the Postgresql driver.
   --  ------------------------------
   overriding
   procedure Finalize (D : in out Postgresql_Driver) is
      pragma Unreferenced (D);
   begin
      Log.Debug ("Deleting the Postgresql driver");
   end Finalize;

end ADO.Drivers.Connections.Postgresql;
