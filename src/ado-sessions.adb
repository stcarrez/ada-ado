-----------------------------------------------------------------------
--  ado-sessions -- Sessions Management
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with ADO.Sequences;
with ADO.Statements.Create;
package body ADO.Sessions is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Sessions");

   procedure Check_Session (Database : in Session'Class;
                            Message  : in String := "") is
   begin
      if Database.Impl = null then
         Log.Error ("Session is closed or not initialized");
         raise Session_Error;
      end if;
      if Message'Length > 0 then
         Log.Info (Message, Database.Impl.Database.Value.Ident);
      end if;
   end Check_Session;

   --  ---------
   --  Session
   --  ---------

   --  ------------------------------
   --  Get the session status.
   --  ------------------------------
   function Get_Status (Database : in Session) return Connection_Status is
   begin
      if Database.Impl = null or else Database.Impl.Database.Is_Null then
         return CLOSED;
      else
         return OPEN;
      end if;
   end Get_Status;

   --  ------------------------------
   --  Get the database driver which manages this connection.
   --  ------------------------------
   function Get_Driver (Database : in Session) return ADO.Connections.Driver_Access is
   begin
      if Database.Impl = null or else Database.Impl.Database.Is_Null then
         return null;
      else
         return Database.Impl.Database.Value.Get_Driver;
      end if;
   end Get_Driver;

   --  ------------------------------
   --  Close the session.
   --  ------------------------------
   procedure Close (Database : in out Session) is

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Session_Record,
                                    Name   => Session_Record_Access);

      Is_Zero : Boolean;
   begin
      Log.Info ("Closing session");
      if Database.Impl /= null then
         ADO.Objects.Release_Proxy (Database.Impl.Proxy);
         Database.Impl.Database.Value.Close;
         Util.Concurrent.Counters.Decrement (Database.Impl.Counter, Is_Zero);
         if Is_Zero then
            Free (Database.Impl);
         end if;
         Database.Impl := null;
      end if;
   end Close;

   --  ------------------------------
   --  Insert a new cache in the manager.  The cache is identified by the given name.
   --  ------------------------------
   procedure Add_Cache (Database : in out Session;
                        Name     : in String;
                        Cache    : in ADO.Caches.Cache_Type_Access) is
   begin
      Check_Session (Database);
      Database.Impl.Values.Add_Cache (Name, Cache);
   end Add_Cache;

   --  ------------------------------
   --  Attach the object to the session.
   --  ------------------------------
   procedure Attach (Database : in out Session;
                     Object   : in ADO.Objects.Object_Ref'Class) is
      pragma Unreferenced (Object);
   begin
      Check_Session (Database);
   end Attach;

   --  ------------------------------
   --  Check if the session contains the object identified by the given key.
   --  ------------------------------
   function Contains (Database : in Session;
                      Item     : in ADO.Objects.Object_Key) return Boolean is
   begin
      Check_Session (Database);
      return ADO.Objects.Cache.Contains (Database.Impl.Cache, Item);
   end Contains;

   --  ------------------------------
   --  Remove the object from the session cache.
   --  ------------------------------
   procedure Evict (Database : in out Session;
                    Item     : in ADO.Objects.Object_Key) is
   begin
      Check_Session (Database);
      ADO.Objects.Cache.Remove (Database.Impl.Cache, Item);
   end Evict;

   --  ------------------------------
   --  Create a query statement.  The statement is not prepared
   --  ------------------------------
   function Create_Statement (Database : in Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement is
   begin
      Check_Session (Database);
      declare
         Query : constant Query_Statement_Access
           := Database.Impl.Database.Value.Create_Statement (Table);
      begin
         return ADO.Statements.Create.Create_Statement (Query, Database.Impl.Values.all'Access);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create a query statement.  The statement is not prepared
   --  ------------------------------
   function Create_Statement (Database : in Session;
                              Query    : in String)
                              return Query_Statement is
   begin
      Check_Session (Database);
      declare
         Stmt : constant Query_Statement_Access
           := Database.Impl.Database.Value.Create_Statement (Query);
      begin
         return ADO.Statements.Create.Create_Statement (Stmt, Database.Impl.Values.all'Access);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create a query statement.  The statement is not prepared
   --  ------------------------------
   function Create_Statement (Database : in Session;
                              Query    : in ADO.Queries.Context'Class)
                              return Query_Statement is
   begin
      Check_Session (Database);
      declare
         SQL  : constant String := Query.Get_SQL (Database.Impl.Queries.all);
         Stmt : Query_Statement := Database.Create_Statement (SQL);
      begin
         Stmt.Set_Parameters (Query);
         return Stmt;
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create a query statement and initialize the SQL statement with the query definition.
   --  ------------------------------
   function Create_Statement (Database : in Session;
                              Query    : in ADO.Queries.Query_Definition_Access)
                              return Query_Statement is
   begin
      Check_Session (Database);
      declare
         SQL   : constant String := ADO.Queries.Get_SQL (Query, Database.Impl.Queries.all, False);
      begin
         return Database.Create_Statement (SQL);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create a query statement.  The statement is not prepared
   --  ------------------------------
   function Create_Statement (Database : in Session;
                              Query    : in ADO.SQL.Query'Class;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Query_Statement is
   begin
      Check_Session (Database);
      declare
         Stmt : Query_Statement := Database.Create_Statement (Table);
      begin
         if Query in ADO.Queries.Context'Class then
            declare
               SQL : constant String
                 := ADO.Queries.Context'Class (Query).Get_SQL (Database.Impl.Queries.all);
            begin
               ADO.SQL.Append (Stmt.Get_Query.SQL, SQL);
            end;
         end if;
         Stmt.Set_Parameters (Query);
         return Stmt;
      end;
   end Create_Statement;

   --  ------------------------------
   --  Execute the SQL statement.
   --  ------------------------------
   procedure Execute (Database : in Session'Class;
                      SQL      : in String) is
      Stmt : ADO.Statements.Query_Statement
         := Database.Create_Statement (SQL);
   begin
      Stmt.Execute;
   end Execute;

   --  ------------------------------
   --  Load the database schema definition for the current database.
   --  ------------------------------
   procedure Load_Schema (Database : in Session;
                          Schema   : out ADO.Schemas.Schema_Definition) is
   begin
      Check_Session (Database, "Loading schema {0}");
      Database.Impl.Database.Value.Load_Schema (Schema);
   end Load_Schema;

   --  ------------------------------
   --  Check if the table with the given name exists in the database.
   --  ------------------------------
   function Has_Table (Database : in Session;
                       Name     : in String) return Boolean is
   begin
      Check_Session (Database, "Has_Table {0}");
      return Database.Impl.Database.Value.Has_Table (Name);
   end Has_Table;

   --  ---------
   --  Master Session
   --  ---------

   --  ------------------------------
   --  Start a transaction.
   --  ------------------------------
   procedure Begin_Transaction (Database : in out Master_Session) is
   begin
      Check_Session (Database, "Begin transaction {0}");
      Database.Impl.Database.Value.Begin_Transaction;
   end Begin_Transaction;

   --  ------------------------------
   --  Commit the current transaction.
   --  ------------------------------
   procedure Commit (Database : in out Master_Session) is
   begin
      Check_Session (Database, "Commit transaction {0}");
      Database.Impl.Database.Value.Commit;
   end Commit;

   --  ------------------------------
   --  Rollback the current transaction.
   --  ------------------------------
   procedure Rollback (Database : in out Master_Session) is
   begin
      Check_Session (Database, "Rollback transaction {0}");
      Database.Impl.Database.Value.Rollback;
   end Rollback;

   --  ------------------------------
   --  Allocate an identifier for the table.
   --  ------------------------------
   procedure Allocate (Database : in out Master_Session;
                       Id       : in out ADO.Objects.Object_Record'Class) is
   begin
      Check_Session (Database);
      ADO.Sequences.Allocate (Database.Sequences.all, Id);
   end Allocate;

   --  ------------------------------
   --  Flush the objects that were modified.
   --  ------------------------------
   procedure Flush (Database : in out Master_Session) is
   begin
      Check_Session (Database);
   end Flush;

   overriding
   procedure Adjust (Object : in out Session) is
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Increment (Object.Impl.Counter);
      end if;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Session) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Session_Record,
                                         Name   => Session_Record_Access);
      Is_Zero : Boolean;
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Decrement (Object.Impl.Counter, Is_Zero);
         if Is_Zero then
            ADO.Objects.Release_Proxy (Object.Impl.Proxy, Detach => True);
            if not Object.Impl.Database.Is_Null then
               Object.Impl.Database.Value.Close;
            end if;
            Free (Object.Impl);
         end if;
         Object.Impl := null;
      end if;
   end Finalize;

   --  ------------------------------
   --  Create a delete statement
   --  ------------------------------
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Delete_Statement is
   begin
      Check_Session (Database);
      declare
         Stmt : constant Delete_Statement_Access
           := Database.Impl.Database.Value.Create_Statement (Table);
      begin
         return ADO.Statements.Create.Create_Statement (Stmt.all'Access,
                                                        Database.Impl.Values.all'Access);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create an update statement
   --  ------------------------------
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Update_Statement is
   begin
      Check_Session (Database);
      declare
         Stmt : constant Update_Statement_Access
           := Database.Impl.Database.Value.Create_Statement (Table);
      begin
         return ADO.Statements.Create.Create_Statement (Stmt.all'Access,
                                                        Database.Impl.Values.all'Access);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Create an insert statement
   --  ------------------------------
   function Create_Statement (Database : in Master_Session;
                              Table    : in ADO.Schemas.Class_Mapping_Access)
                              return Insert_Statement is
   begin
      Check_Session (Database);
      declare
         Stmt : constant Insert_Statement_Access
           := Database.Impl.Database.Value.Create_Statement (Table);
      begin
         return ADO.Statements.Create.Create_Statement (Stmt.all'Access,
                                                        Database.Impl.Values.all'Access);
      end;
   end Create_Statement;

   --  ------------------------------
   --  Get the audit manager.
   --  ------------------------------
   function Get_Audit_Manager (Database : in Master_Session)
                               return access Audits.Audit_Manager'Class is
   begin
      return Database.Audit;
   end Get_Audit_Manager;

   --  ------------------------------
   --  Internal operation to get access to the database connection.
   --  ------------------------------
   procedure Access_Connection (Database : in out Master_Session;
                                Process  : not null access
                                  procedure (Connection : in out Database_Connection'Class)) is
   begin
      Check_Session (Database);
      Process (Database.Impl.Database.Value);
   end Access_Connection;

   --  ------------------------------
   --  Internal method to get the session proxy associated with the given database session.
   --  The session proxy is associated with some ADO objects to be able to retrieve the database
   --  session for the implementation of lazy loading.  The session proxy is kept until the
   --  session exist and at least one ADO object is refering to it.
   --  ------------------------------
   function Get_Session_Proxy (Database : in Session) return ADO.Objects.Session_Proxy_Access is
      use type ADO.Objects.Session_Proxy_Access;
   begin
      Check_Session (Database);
      if Database.Impl.Proxy = null then
         Database.Impl.Proxy := ADO.Objects.Create_Session_Proxy (Database.Impl);
      end if;
      return Database.Impl.Proxy;
   end Get_Session_Proxy;

end ADO.Sessions;
