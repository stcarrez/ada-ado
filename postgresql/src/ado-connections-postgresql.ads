-----------------------------------------------------------------------
--  ado-connections-postgresql -- Postgresql Database connections
--  Copyright (C) 2018, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private with PQ;
private with Ada.Strings.Unbounded;
package ADO.Connections.Postgresql is

   type Postgresql_Driver is limited private;

   --  Initialize the Postgresql driver.
   procedure Initialize;

private

   use Ada.Strings.Unbounded;

   --  Create a new Postgresql connection using the configuration parameters.
   procedure Create_Connection (D      : in out Postgresql_Driver;
                                Config : in Configuration'Class;
                                Result : in out Ref.Ref'Class);

   type Postgresql_Driver is new ADO.Connections.Driver with record
      Id : Natural := 0;
   end record;

   --  Create the database and initialize it with the schema SQL file.
   --  The `Admin` parameter describes the database connection with administrator access.
   --  The `Config` parameter describes the target database connection.
   overriding
   procedure Create_Database (D           : in out Postgresql_Driver;
                              Admin       : in Configs.Configuration'Class;
                              Config      : in Configs.Configuration'Class;
                              Schema_Path : in String;
                              Messages    : out Util.Strings.Vectors.Vector);

   --  Deletes the Postgresql driver.
   overriding
   procedure Finalize (D : in out Postgresql_Driver);

   --  Database connection implementation
   type Database_Connection is new ADO.Connections.Database_Connection with record
      Name        : Unbounded_String;
      Server_Name : Unbounded_String;
      Login_Name  : Unbounded_String;
      Password    : Unbounded_String;
      Server      : PQ.PGconn_Access := PQ.Null_PGconn;
      Connected   : Boolean      := False;
   end record;

   type Database_Connection_Access is access all Database_Connection'Class;

   --  Get the database driver which manages this connection.
   overriding
   function Get_Driver (Database : in Database_Connection)
                        return Driver_Access;

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

   --  Check if the table with the given name exists in the database.
   overriding
   function Has_Table (Database : in Database_Connection;
                       Name     : in String) return Boolean;

end ADO.Connections.Postgresql;
