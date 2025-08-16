-----------------------------------------------------------------------
--  ado-sequences -- Database sequence generator
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with ADO.Sessions;
with ADO.Objects;
with Util.Strings.Maps;
with Ada.Strings.Unbounded;
limited with ADO.Sessions.Factory;

--  == Sequence Generators ==
--  The sequence generator is responsible for creating unique ID's
--  across all database objects.  Each table can be associated with a sequence generator
--  which is configured on the session factory.  The sequence generators are shared by
--  several database sessions and the implementation is thread-safe.  The sequence generator
--  provides an efficient alternative to the `AUTO INCREMENT` support provided by SQL databases.
--
--  The `HiLo_Generator` implements a simple High Low sequence generator
--  by using sequences that avoid to access the database.
--  The `Snowflake_Generator` implements an efficient algorithm that can be used in
--  distributed environments.
--
--  For example, to allocate a new database identifier for a table `user`, you could write:
--
--    DB : ADO.Sessions.Master_Session := ...;
--    Id : ADO.Identifier;
--    ...
--      DB.Allocate (Name => "user", Id => Id);
--
--  @include ado-sequences-hilo.ads
--  @include ado-sequences-snowflake.ads
package ADO.Sequences is

   --  Exception raised when the sequence generator fails to allocate an id.
   Allocate_Error : exception;

   type Session_Factory_Access is access all ADO.Sessions.Factory.Session_Factory'Class;

   --  ------------------------------
   --  Abstract sequence generator
   --  ------------------------------
   type Generator (Name_Length : Natural) is
     abstract new Ada.Finalization.Limited_Controlled with private;
   type Generator_Access is access all Generator'Class;

   --  Allocate an identifier using the generator.
   procedure Allocate (Gen     : in out Generator;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out Identifier) is abstract;

   --  Get a session to connect to the database.
   function Get_Session (Gen : in Generator) return ADO.Sessions.Master_Session'Class;

   type Generator_Factory is access
     function (Sess_Factory : in Session_Factory_Access;
               Name         : in String)
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
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out Objects.Object_Record'Class);

   procedure Allocate (Manager : in out Factory;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Name    : in String;
                       Id      : out Identifier);

   --  Set a generator to be used for the given sequence.
   procedure Set_Generator (Manager : in out Factory;
                            Gen     : in Generator_Access);

   procedure Set_Generator (Manager : in out Factory;
                            Name    : in String;
                            Gen     : in String);

   --  Set the default factory for creating generators.
   --  The default factory is the HiLo generator.
   procedure Set_Default_Generator (Manager      : in out Factory;
                                    Factory      : in Generator_Factory;
                                    Sess_Factory : in Session_Factory_Access;
                                    Use_New_Session : in Boolean);

   --  Set the name of a sequence generator to be used by default when a generator is
   --  not found or was not configured by using `Set_Generator`.
   procedure Set_Default_Generator (Manager : in out Factory;
                                    Name    : in String);

private

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   type Generator (Name_Length : Natural) is
     abstract new Ada.Finalization.Limited_Controlled with
      record
         Factory         : Session_Factory_Access;
         Use_New_Session : Boolean := True;
         Name            : String (1 .. Name_Length);
      end record;

   --  Each sequence generator is accessed through a protected type
   --  to make sure the allocation is unique and works in multi-threaded
   --  environments.
   protected type Sequence_Generator is

      --  Allocate a unique identifier for the given sequence.
      procedure Allocate (Session : in out Sessions.Master_Session'Class;
                          Id      : out Identifier);

      procedure Set_Generator (Gen  : in Generator_Access);

   private
      Generator : Generator_Access;
   end Sequence_Generator;
   type Sequence_Generator_Access is access all Sequence_Generator;

   --  Map to keep track of allocation generators for each sequence.
   package Sequence_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type     => String,
      Element_Type => Sequence_Generator_Access,
      Hash         => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   --  The sequence factory map is also accessed through a protected type.
   protected type Factory_Map is

      --  Get the sequence generator associated with the name.
      --  If there is no such generator, an entry is created by using
      --  the default generator.
      procedure Get_Generator (Name : in String;
                               Check_Alias : in Boolean;
                               Seq  : out Sequence_Generator_Access);

      --  Set the sequence generator associated with the name.
      procedure Set_Generator (Gen  : in Generator_Access);

      procedure Set_Generator (Name : in String;
                               Gen  : in String);

      --  Set the default sequence generator.
      procedure Set_Default_Generator (Gen : in Generator_Factory;
                                       Factory : in Session_Factory_Access;
                                       Gen_Use_New_Session : in Boolean);
      procedure Set_Default_Generator (Name : in String);

      --  Clear the factory map.
      procedure Clear;
   private
      Map               : Sequence_Maps.Map;
      Alias             : Util.Strings.Maps.Map;
      Create_Generator  : Generator_Factory;
      Default_Generator : UString;
      Sess_Factory      : Session_Factory_Access;
      Use_New_Session   : Boolean := True;
   end Factory_Map;

   type Factory is new Ada.Finalization.Limited_Controlled with record
      Map : Factory_Map;
   end record;

   overriding
   procedure Finalize (Manager : in out Factory);

end ADO.Sequences;
