-----------------------------------------------------------------------
--  awa --
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

with Ada.Unchecked_Deallocation;

package body ADO.Model is

   procedure Set_Field (Object : in out Sequence_Ref'Class;
                        Impl   : out Sequence_Ref_Access;
                        Field  : in Positive) is
   begin
      if Object.Is_Null then
         Impl := new Sequence_Ref_Impl;
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      else
         Impl := Sequence_Ref_Impl (Object.Get_Object.all)'Access;
      end if;
      Impl.Modified (Field) := True;
   end Set_Field;

   -- ----------------------------------------
   --  Data object: Sequence_Ref
   -- ----------------------------------------
   procedure Set_Name (Object : in out Sequence_Ref;
                       Value : in String) is
   begin
      Object.Set_Name (To_Unbounded_String (Value));
   end Set_Name;

   procedure Set_Name (Object : in out Sequence_Ref;
                        Value : in Unbounded_String) is
      Impl : Sequence_Ref_Access;
   begin
      Set_Field (Object, Impl, 1);
      Impl.Name := Value;
   end Set_Name;

   function Get_Name (Object : in Sequence_Ref)
                 return String is
   begin
      return To_String (Object.Get_Name);
   end Get_Name;

   function Get_Name (Object : in Sequence_Ref)
                  return Unbounded_String is
      Impl : constant Sequence_Ref_Access := Sequence_Ref_Impl (Object.Get_Object.all)'Access;
   begin
      return Impl.Name;
   end Get_Name;

   procedure Set_Value (Object : in out Sequence_Ref;
                         Value : in Identifier) is
      Impl : Sequence_Ref_Access;
   begin
      Set_Field (Object, Impl, 2);
      Impl.Value := Value;
   end Set_Value;

   function Get_Value (Object : in Sequence_Ref)
                  return Identifier is
      Impl : constant Sequence_Ref_Access := Sequence_Ref_Impl (Object.Get_Object.all)'Access;
   begin
      return Impl.Value;
   end Get_Value;

   procedure Set_Block_Size (Object : in out Sequence_Ref;
                              Value : in Identifier) is
      Impl : Sequence_Ref_Access;
   begin
      Set_Field (Object, Impl, 3);
      Impl.Block_Size := Value;
   end Set_Block_Size;

   function Get_Block_Size (Object : in Sequence_Ref)
                  return Identifier is
      Impl : constant Sequence_Ref_Access := Sequence_Ref_Impl (Object.Get_Object.all)'Access;
   begin
      return Impl.Block_Size;
   end Get_Block_Size;

   --  Copy of the object.
   function Copy (Object : Sequence_Ref) return Sequence_Ref is
      Result : Sequence_Ref;
   begin
      if not Object.Is_Null then
         declare
            Impl : constant Sequence_Ref_Access
              := Sequence_Ref_Impl (Object.Get_Object.all)'Access;
            Copy : constant Sequence_Ref_Access
              := new Sequence_Ref_Impl;
         begin
            ADO.Objects.Set_Object (Result, Copy.all'Access);
            Copy.Name := Impl.Name;
            Copy.Value := Impl.Value;
            Copy.Block_Size := Impl.Block_Size;
            Copy.Version := Impl.Version;
         end;
      end if;
      return Result;
   end Copy;

   procedure Find (Object     : in out Sequence_Ref;
                   Database   : in out ADO.Sessions.Session'Class;
                   Parameters : in ADO.Statements.Parameter_List;
                   Found      : out Boolean) is
      Impl  : constant Sequence_Ref_Access := new Sequence_Ref_Impl;
   begin
      Impl.Find (Database, Parameters, Found);
      if Found then
         ADO.Objects.Set_Object (Object, Impl.all'Access);
      else
         ADO.Objects.Set_Object (Object, null);
         Destroy (Impl);
      end if;
   end Find;

   procedure Load (Object   : in out Sequence_Ref;
                   Database : in out ADO.Sessions.Session'Class;
                   Id       : in String) is
      Impl  : constant Sequence_Ref_Access := new Sequence_Ref_Impl;
      Found : Boolean;
      Params : ADO.Statements.Parameter_List;
   begin
      Params.Bind_Param (Position => 1, Value => Id);
      Params.Set_Filter ("name = ?");
      Impl.Find (Database, Params, Found);
      if not Found then
         Destroy (Impl);
         raise ADO.Databases.NOT_FOUND;
      end if;
      ADO.Objects.Set_Object (Object, Impl.all'Access);
   end Load;

   procedure Save (Object   : in out Sequence_Ref;
                   Database : in out ADO.Sessions.Master_Session'Class) is
      Impl : ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl = null then
         Impl := new Sequence_Ref_Impl;
         ADO.Objects.Set_Object (Object, Impl);
      end if;
      if not Is_Created (Impl.all) then
         Impl.Create (Database);
      else
         Impl.Save (Database);
      end if;
   end Save;

   procedure Delete (Object   : in out Sequence_Ref;
                     Database : in out ADO.Sessions.Master_Session'Class) is
      Impl : constant ADO.Objects.Object_Record_Access := Object.Get_Object;
   begin
      if Impl /= null then
         Impl.Delete (Database);
      end if;
   end Delete;

   --  --------------------
   --  Free the object
   --  --------------------
   procedure Destroy (Object : access Sequence_Ref_Impl) is
      type Sequence_Ref_Impl_Ptr is access all Sequence_Ref_Impl;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Sequence_Ref_Impl, Sequence_Ref_Impl_Ptr);

      Ptr : Sequence_Ref_Impl_Ptr := Sequence_Ref_Impl (Object.all)'Access;
   begin
      Unchecked_Free (Ptr);
   end Destroy;

   procedure Find (Object     : in out Sequence_Ref_Impl;
                   Database   : in out ADO.Sessions.Session'Class;
                   Parameters : in ADO.Statements.Parameter_List;
                   Found      : out Boolean) is
      SQL : constant String := "SELECT name, value, block_size, version FROM sequence AS o ";
      Query : Query_Statement'Class
          := Database.Create_Statement (SQL);
   begin
      Query.Set_Parameters (Parameters);
      Query.Execute;
      if Query.Has_Elements then
         Object.Load (Query);
         Query.Next;
         Found := not Query.Has_Elements;
      else
         Found := False;
      end if;
   end Find;

   procedure Save (Object   : in out Sequence_Ref_Impl;
                   Database : in out ADO.Sessions.Master_Session'Class) is
      SQL   : constant String := "UPDATE sequence SET ";
      Query : Statement'Class := Database.Create_Statement (SQL);
      Pos   : Natural := 0;
   begin
      if Object.Modified (1) then
         Pos := Pos + 1;
         if Pos > 1 then
            Append (Query => Query, SQL => ",");
         end if;
         Append (Query => Query, SQL => "name = ?");
         Query.Bind_Param (Position => Pos, Value => Object.Name);
         Object.Modified (1) := False;
      end if;
      if Object.Modified (2) then
         Pos := Pos + 1;
         if Pos > 1 then
            Append (Query => Query, SQL => ",");
         end if;
         Append (Query => Query, SQL => "value = ?");
         Query.Bind_Param (Position => Pos, Value => Object.Value);
         Object.Modified (2) := False;
      end if;
      if Object.Modified (3) then
         Pos := Pos + 1;
         if Pos > 1 then
            Append (Query => Query, SQL => ",");
         end if;
         Append (Query => Query, SQL => "block_size = ?");
         Query.Bind_Param (Position => Pos, Value => Object.Block_Size);
         Object.Modified (3) := False;
      end if;
      if Pos > 0 then
         Object.Version := Object.Version + 1;
         Query.Append (SQL => ", version = ?");
         Query.Bind_Param (Position => Pos + 1, Value => Object.Version);
         Query.Append (SQL => " where name = ? and version = ?");
         Query.Bind_Param (Position => Pos + 2, Value => Object.Name);
         Query.Bind_Param (Position => Pos + 3, Value => Object.Version - 1);
         declare
            Result : Integer;
         begin
            Query.Execute (Result);
            if Result /= 1 then
               if Result = 0 then
                  raise LAZY_LOCK;
               else
                  raise UPDATE_ERROR;
               end if;
            end if;
         end;
      end if;
   end Save;

   procedure Create (Object   : in out Sequence_Ref_Impl;
                     Database : in out ADO.Sessions.Master_Session'Class) is
      SQL   : constant String := "INSERT INTO sequence (name, value, block_size, version) VALUES(?,?,?,?);";
      Query : ADO.Statements.Query_Statement'Class
                  := Database.Create_Statement (SQL);
      Result : Integer;
   begin
      Query.Bind_Param (Position => 1, Value => Object.Name);
      Query.Bind_Param (Position => 2, Value => Object.Value);
      Query.Bind_Param (Position => 3, Value => Object.Block_Size);
      Object.Version := 1;
      Query.Bind_Param (Position => 4, Value => Object.Version);
      Query.Execute (Result);
      if Result /= 1 then
         raise INSERT_ERROR;
      end if;
      Set_Created (Object);
   end Create;

   procedure Delete (Object   : in out Sequence_Ref_Impl;
                     Database : in out ADO.Sessions.Master_Session'Class) is
      SQL : constant String := "DELETE FROM sequence WHERE name = ?";
      Query : ADO.Statements.Query_Statement'Class := Database.Create_Statement (SQL);
   begin
      Query.Bind_Param (Position => 1, Value => Object.Name);
      Query.Execute;
   end Delete;

   function Get_Value (Item : in Sequence_Ref;
                       Name : in String) return EL.Objects.Object is
      Impl : constant access Sequence_Ref_Impl := Sequence_Ref_Impl (Item.Get_Object.all)'Access;
   begin
      if Name = "name" then
         return EL.Objects.To_Object (Impl.Name);
      end if;
      if Name = "value" then
         return EL.Objects.To_Object (Long_Long_Integer (Impl.Value));
      end if;
      if Name = "block_size" then
         return EL.Objects.To_Object (Long_Long_Integer (Impl.Block_Size));
      end if;
      raise ADO.Databases.NOT_FOUND;
   end Get_Value;

   procedure List (Object   : in out Sequence_Ref_Vector;
                   Query    : in out ADO.Statements.Query_Statement'Class) is
   begin
      Query.Prepend ("SELECT name, value, block_size, version FROM sequence AS o ");
      Query.Execute;
      Sequence_Ref_Vectors.Clear (Object);
      while Query.Has_Elements loop
         declare
            Item : Sequence_Ref;
            Impl : constant Sequence_Ref_Access := new Sequence_Ref_Impl;
         begin
            Impl.Load (Query);
            ADO.Objects.Set_Object (Item, Impl.all'Access);
            Object.Append (Item);
         end;
         Query.Next;
      end loop;
   end List;

   --  ------------------------------
   --  Load the object from current iterator position
   --  ------------------------------
   procedure Load (Object : in out Sequence_Ref_Impl;
                   Query  : in out ADO.Statements.Query_Statement'Class) is
   begin
      Object.Name := Query.Get_Unbounded_String (1);
      Object.Value := Query.Get_Identifier (2);
      Object.Block_Size := Query.Get_Identifier (3);
      Object.Version := Query.Get_Integer ( 4);
      Set_Created (Object);
   end Load;



end ADO.Model;
