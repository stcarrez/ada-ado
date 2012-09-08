-----------------------------------------------------------------------
--  ADO Sequences -- Database sequence generator
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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
with Util.Strings;
with ADO.Sessions.Factory;
with Ada.Unchecked_Deallocation;
package body ADO.Sequences is

   use Util.Log;
   use Sequence_Maps;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Sequences");

   procedure Free is new
     Ada.Unchecked_Deallocation (Object => ADO.Sequences.Sequence_Generator,
                                 Name   => ADO.Sequences.Sequence_Generator_Access);

   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Generator'Class,
                                 Name   => Generator_Access);

   --  ------------------------------
   --  Get the name of the sequence.
   --  ------------------------------
   function Get_Sequence_Name (Gen : in Generator'Class) return String is
   begin
      return To_String (Gen.Name);
   end Get_Sequence_Name;

   --  ------------------------------
   --  Get a session to connect to the database.
   --  ------------------------------
   function Get_Session (Gen : in Generator) return ADO.Sessions.Master_Session'Class is
   begin
      return Gen.Factory.Get_Master_Session;
   end Get_Session;

   protected body Sequence_Generator is

      --  ------------------------------
      --  Allocate a unique identifier for the given sequence.
      --  ------------------------------
      procedure Allocate (Id : in out Objects.Object_Record'Class) is
      begin
         Generator.Allocate (Id);
      end Allocate;

      procedure Set_Generator (Name : in Unbounded_String;
                               Gen  : in Generator_Access) is
      begin
         Gen.Name  := Name;
         Generator := Gen;
      end Set_Generator;

      procedure Clear is
      begin
         Free (Generator);
      end Clear;

   end Sequence_Generator;

   --  ------------------------------
   --  Allocate a unique identifier for the given table.
   --  ------------------------------
   procedure Allocate (Manager : in out Factory;
                       Id      : in out ADO.Objects.Object_Record'Class) is
      Gen  : Sequence_Generator_Access;
      Name : constant Util.Strings.Name_Access := Id.Get_Table_Name;
   begin
      Manager.Map.Get_Generator (To_Unbounded_String (Name.all), Gen);
      Gen.Allocate (Id);
   end Allocate;

   --  ------------------------------
   --  Set a generator to be used for the given sequence.
   --  ------------------------------
   procedure Set_Generator (Manager : in out Factory;
                            Name    : in String;
                            Gen     : in Generator_Access) is
      N  : constant Unbounded_String := To_Unbounded_String (Name);
      G  : constant Sequence_Generator_Access := new Sequence_Generator;
   begin
      G.Set_Generator (N, Gen);
      Manager.Map.Set_Generator (N, G);
   end Set_Generator;

   --  ------------------------------
   --  Set the default factory for creating generators.
   --  The default factory is the HiLo generator.
   --  ------------------------------
   procedure Set_Default_Generator
     (Manager      : in out Factory;
      Factory      : in Generator_Factory;
      Sess_Factory : in Session_Factory_Access) is
   begin
      Manager.Map.Set_Default_Generator (Factory, Sess_Factory);
   end Set_Default_Generator;

   --  The sequence factory map is also accessed through a protected type.
   protected body Factory_Map is

      --  ------------------------------
      --  Get the sequence generator associated with the name.
      --  If there is no such generator, an entry is created by using
      --  the default generator.
      --  ------------------------------
      procedure Get_Generator (Name : in Unbounded_String;
                               Gen  : out Sequence_Generator_Access) is
         Pos : constant Cursor := Find (Map, Name);
      begin
         if not Has_Element (Pos) then
            Log.Info ("Creating sequence generator for {0}", To_String (Name));

            Gen := new Sequence_Generator;
            Gen.Set_Generator (Name, Create_Generator.all (Sess_Factory));
            Insert (Map, Name, Gen);
         else
            Gen := Element (Pos);
         end if;
      end Get_Generator;

      --  ------------------------------
      --  Set the sequence generator associated with the name.
      --  ------------------------------
      procedure Set_Generator (Name : in Unbounded_String;
                               Gen  : in Sequence_Generator_Access) is
         Pos : constant Cursor := Find (Map, Name);
      begin
         Log.Info ("Setting sequence generator for {0}", To_String (Name));

         if not Has_Element (Pos) then
            Insert (Map, Name, Gen);
         else
            declare
               Node : Sequence_Generator_Access := Element (Pos);
            begin
               Node.Clear;
               Free (Node);
            end;
            Replace_Element (Map, Pos, Gen);
         end if;
      end Set_Generator;

      --  ------------------------------
      --  Set the default sequence generator.
      --  ------------------------------
      procedure Set_Default_Generator
        (Gen     : in Generator_Factory;
         Factory : in Session_Factory_Access) is
      begin
         Create_Generator := Gen;
         Sess_Factory := Factory;
      end Set_Default_Generator;

      --  ------------------------------
      --  Clear the factory map.
      --  ------------------------------
      procedure Clear is
      begin
         Log.Info ("Clearing the sequence factory");

         loop
            declare
               Pos  : Cursor := Map.First;
               Node : Sequence_Generator_Access;
            begin
               exit when not Has_Element (Pos);
               Node := Element (Pos);
               Map.Delete (Pos);
               Node.all.Clear;
               Free (Node);
            end;
         end loop;
      end Clear;

   end Factory_Map;

   procedure Finalize (Manager : in out Factory) is
   begin
      Manager.Map.Clear;
   end Finalize;

end ADO.Sequences;
