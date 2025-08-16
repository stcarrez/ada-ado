-----------------------------------------------------------------------
--  ado-sequences-snowflake-- Snowflake ID generator
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces;

--  === Snowflake ID Generator ===
--  The snowflake generator is a form of unique ID generator that can be used
--  in distributed environment.  It generates a 64-bit number decomposed in a
--  first 42 bits representing a timestamp since a given epoch in milliseconds.
--  The next N bits represent the machine ID and the last P bits a sequence
--  number.  Some snowflake generators are using 10 bits for the machine ID and
--  12 bits for the sequence number.  Some others are using 13 bits for a shard ID
--  and 10 bits for the sequence number.  To give some flexibility, the snowflake
--  generator is provided as a generic package that must be instantiated with
--  two constants:
--
--  - a Node_Bits representing the number of bits for the machine ID, shard ID or node ID,
--  - a Sequence_Bits representing the number of bits for the sequence.
--
--  The timestamp will use `64 - Node_Bits - Sequence_Bits` bits.
--
--  For example, the package is first instantiated with the desired configuration:
--
--     package My_Snowflake is
--        new ADO.Sequences.Snowflake (Node_Bits => 12,
--                                     Sequence_Bits => 10);
--
--  An instance of the generator must be created and registered in the session factory.
--  The generator is assigned a name (in most cases a database table name) which will be used
--  to identify it by the `Allocate` procedure.  The Snowflake generator needs an epoch
--  timestamp representing the start of epoch for the sequence generation and a machine ID or
--  node ID which should be unique and specific to each server instance that will create and
--  use such generator.
--
--     S : ADO.Sequences.Generator_Access
--         := My_Snowflake.Create_Snowflake_Generator
--               (Sess_Factory'Unchecked_Access, "user", Epoch, 10);
--     begin
--        Sess_Factory.Set_Generator (S);
generic
   Node_Bits     : Positive := 10;
   Sequence_Bits : Positive := 12;
package ADO.Sequences.Snowflake is

   use type Interfaces.Unsigned_64;

   Sequence_Last  : Natural := 2 ** Sequence_Bits - 1;
   Node_Last      : Natural := 2 ** Node_Bits - 1;
   Timestamp_Last : Interfaces.Unsigned_64 := 2 ** (64 - Node_Bits - Sequence_Bits) - 1;

   type Node_Id is new Natural range 0 .. Node_Last;
   type Timestamp_Type is new Interfaces.Unsigned_64 range 0 .. Timestamp_Last;
   type Sequence_Type is new Natural range 0 .. Sequence_Last;

   --  ------------------------------
   --  Snowflake ID generator
   --  ------------------------------
   type Snowflake_Generator (Name_Length : Natural) is
     new Generator with private with Dynamic_Predicate => Sequence_Bits + Node_Bits < 32;

   --  Allocate an identifier using the generator.
   overriding
   procedure Allocate (Gen     : in out Snowflake_Generator;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out Identifier);

   function Create_Snowflake_Generator
     (Sess_Factory : in Session_Factory_Access;
      Name         : in String;
      Epoch        : in Ada.Calendar.Time;
      Node         : in Node_Id)
      return Generator_Access;

private

   type Snowflake_Generator (Name_Length : Natural) is new Generator (Name_Length) with record
      Node          : Identifier := 0;
      Epoch         : Ada.Calendar.Time;
      Cur_Tms       : Timestamp_Type := 0;
      Cur_Timestamp : Identifier := 0;
      Sequence      : Sequence_Type := 0;
   end record;

end ADO.Sequences.Snowflake;
