-----------------------------------------------------------------------
--  ado-schemas-entities -- Entity types cache
--  Copyright (C) 2011, 2012, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;
with Util.Strings;

with ADO.SQL;
with ADO.Statements;
with ADO.Model;
package body ADO.Schemas.Entities is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Schemas.Entities");

   --  ------------------------------
   --  Expand the name into a target parameter value to be used in the SQL query.
   --  The Expander can return a T_NULL when a value is not found or
   --  it may also raise some exception.
   --  ------------------------------
   overriding
   function Expand (Instance : in out Entity_Cache;
                    Name     : in String) return ADO.Parameters.Parameter is
      Pos : constant Entity_Map.Cursor := Instance.Entities.Find (Name);
   begin
      if not Entity_Map.Has_Element (Pos) then
         Log.Error ("No entity type associated with table {0}", Name);
         raise No_Entity_Type with "No entity type associated with table " & Name;
      end if;
      return ADO.Parameters.Parameter '(T => ADO.Parameters.T_INTEGER,
                                        Len => 0, Value_Len => 0, Position => 0,
                                        Name => "",
                                        Num => Entity_Type'Pos (Entity_Map.Element (Pos)));
   end Expand;

   --  ------------------------------
   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Table : in Class_Mapping_Access) return ADO.Entity_Type is
   begin
      return Find_Entity_Type (Cache, Table.Table);
   end Find_Entity_Type;

   --  ------------------------------
   --  Find the entity type index associated with the given database table.
   --  Raises the No_Entity_Type exception if no such mapping exist.
   --  ------------------------------
   function Find_Entity_Type (Cache : in Entity_Cache;
                              Name  : in Util.Strings.Name_Access) return ADO.Entity_Type is
      Pos : constant Entity_Map.Cursor := Cache.Entities.Find (Name.all);
   begin
      if not Entity_Map.Has_Element (Pos) then
         Log.Error ("No entity type associated with table {0}", Name.all);
         raise No_Entity_Type with "No entity type associated with table " & Name.all;
      end if;
      return Entity_Type (Entity_Map.Element (Pos));
   end Find_Entity_Type;

   --  ------------------------------
   --  Initialize the entity cache by reading the database entity table.
   --  ------------------------------
   procedure Initialize (Cache   : in out Entity_Cache;
                         Session : in out ADO.Sessions.Session'Class) is
      Query : ADO.SQL.Query;
      Stmt  : ADO.Statements.Query_Statement
        := Session.Create_Statement (ADO.Model.ENTITY_TYPE_TABLE'Access);
      Count : Natural := 0;
   begin
      Stmt.Set_Parameters (Query);
      Stmt.Execute;
      while Stmt.Has_Elements loop
         declare
            Id   : constant ADO.Entity_Type := ADO.Entity_Type (Stmt.Get_Integer (0));
            Name : constant String := Stmt.Get_String (1);
         begin
            Cache.Entities.Insert (Key => Name, New_Item => Id);
         end;
         Count := Count + 1;
         Stmt.Next;
      end loop;
      Log.Info ("Loaded {0} table entities", Util.Strings.Image (Count));

   exception
      when others =>
         null;
   end Initialize;

end ADO.Schemas.Entities;
