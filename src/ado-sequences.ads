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

with Ada.Finalization;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with ADO.Sessions;
with ADO.Objects;
limited with ADO.Sessions.Factory;

--  The sequence generator is responsible for creating unique ID's
--  across all database objects.
--
--  Each table can be associated with a sequence generator.
--  The sequence factory is shared by several sessions and the
--  implementation is thread-safe.
--
--  The HiLoGenerator implements a simple High Low sequence generator
--  by using sequences that avoid to access the database.
--
--  Example:
--
--    F  : Factory;
--    Id : Identifier;
--
--    Allocate (Manager => F, Name => "user", Id => Id);
--
package ADO.Sequences is

   type Session_Factory_Access is access all ADO.Sessions.Factory.Session_Factory'Class;

   --  ------------------------------
   --  Abstract sequence generator
   --  ------------------------------
   type Generator is abstract new Ada.Finalization.Limited_Controlled with private;
   type Generator_Access is access all Generator'Class;

   --  Get the name of the sequence.
   function Get_Sequence_Name (Gen : in Generator'Class) return String;

   --  Allocate an identifier using the generator.
   procedure Allocate (Gen : in out Generator;
                       Id  : in out Objects.Object_Record'Class) is abstract;

   --  Get a session to connect to the database.
   function Get_Session (Gen : in Generator) return ADO.Sessions.Master_Session'Class;

   type Generator_Factory is access
     function (Sess_Factory : in Session_Factory_Access)
               return Generator_Access;

   --  ------------------------------
   --  Sequence factory
   --  ------------------------------
   --  The sequence <b>Factory</b> allocates unique ids for new objects.
   --  The factory is shared by all connections to the same database.
   type Factory is limited private;
   type Factory_Access is access all Factory;

   --  Allocate a unique identifier for the given sequence.
   procedure Allocate (Manager : in out Factory;
                       Id      : in out Objects.Object_Record'Class);

   --  Set a generator to be used for the given sequence.
   procedure Set_Generator (Manager : in out Factory;
                            Name    : in String;
                            Gen     : in Generator_Access);

   --  Set the default factory for creating generators.
   --  The default factory is the HiLo generator.
   procedure Set_Default_Generator
     (Manager      : in out Factory;
      Factory      : in Generator_Factory;
      Sess_Factory : in Session_Factory_Access);

private

   use Ada.Strings.Unbounded;

   type Generator is abstract new Ada.Finalization.Limited_Controlled with record
      Name    : Unbounded_String;
      Factory : Session_Factory_Access;
   end record;

   --  Each sequence generator is accessed through a protected type
   --  to make sure the allocation is unique and works in multi-threaded
   --  environments.
   protected type Sequence_Generator is

      --  Allocate a unique identifier for the given sequence.
      procedure Allocate (Id   : in out Objects.Object_Record'Class);

      procedure Set_Generator (Name : in Unbounded_String;
                               Gen  : in Generator_Access);

      --  Free the generator
      procedure Clear;

   private
      Generator : Generator_Access;
   end Sequence_Generator;
   type Sequence_Generator_Access is access all Sequence_Generator;

   --  Map to keep track of allocation generators for each sequence.
   package Sequence_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Sequence_Generator_Access,
      Hash         => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "=" => "=");

   --  The sequence factory map is also accessed through a protected type.
   protected type Factory_Map is

      --  Get the sequence generator associated with the name.
      --  If there is no such generator, an entry is created by using
      --  the default generator.
      procedure Get_Generator (Name : in Unbounded_String;
                               Gen  : out Sequence_Generator_Access);

      --  Set the sequence generator associated with the name.
      procedure Set_Generator (Name : in Unbounded_String;
                               Gen  : in Sequence_Generator_Access);

      --  Set the default sequence generator.
      procedure Set_Default_Generator
        (Gen : in Generator_Factory;
         Factory : in Session_Factory_Access);

      --  Clear the factory map.
      procedure Clear;
   private
      Map              : Sequence_Maps.Map;
      Create_Generator : Generator_Factory;
      Sess_Factory     : Session_Factory_Access;
   end Factory_Map;

   type Factory is new Ada.Finalization.Limited_Controlled with record
      Map : Factory_Map;
   end record;

   overriding
   procedure Finalize (Manager : in out Factory);

end ADO.Sequences;
