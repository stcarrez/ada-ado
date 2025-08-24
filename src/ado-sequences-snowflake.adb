-----------------------------------------------------------------------
--  ado-sequences-snowflake-- Snowflake ID generator
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Calendar.Conversions;
with Util.Log.Loggers;
with ADO.Sessions;

package body ADO.Sequences.Snowflake is

   use Util.Log;
   use Interfaces;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Sequences.Snowflake");

   type Snowflake_Generator_Access is access all Snowflake_Generator'Class;

   --  ------------------------------
   --  Create a high low sequence generator
   --  ------------------------------
   function Create_Snowflake_Generator
     (Sess_Factory : in Session_Factory_Access;
      Name         : in String;
      Epoch        : in Ada.Calendar.Time;
      Node         : in Node_Id)
      return Generator_Access is
      Node_Id  : constant Identifier :=
        Identifier (Shift_Left (Unsigned_64 (Node), Sequence_Bits));
      Epoch_Ns : constant Unsigned_64 :=
        Unsigned_64 (Ada.Calendar.Conversions.To_Unix_Nano_Time (Epoch));
      Result : constant Snowflake_Generator_Access :=
        new Snowflake_Generator '(Ada.Finalization.Limited_Controlled with
                                  Name_Length => Name'Length,
                                  Factory => Sess_Factory,
                                  Name => Name,
                                  Cur_Tms => 0,
                                  Cur_Timestamp => 0,
                                  Epoch => Epoch_Ns,
                                  Node => Node_Id,
                                  others => <>);
   begin
      return Result.all'Access;
   end Create_Snowflake_Generator;

   --  ------------------------------
   --  Allocate an identifier using the generator.
   --  The generator allocates blocks of sequences by using a sequence
   --  table stored in the database.  One database access is necessary
   --  every N allocations.
   --  ------------------------------
   overriding
   procedure Allocate (Gen     : in out Snowflake_Generator;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out Identifier) is
      Now   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      T_ns  : constant Unsigned_64 :=
        Unsigned_64 (Ada.Calendar.Conversions.To_Unix_Nano_Time (Now));
      T_ms  : constant Timestamp_Type :=
        Timestamp_Type ((T_ns - Gen.Epoch) / 1_000_000);
   begin
      if Gen.Cur_Tms /= T_ms then
         Gen.Cur_Tms := T_ms;
         Gen.Sequence := 0;
         Gen.Cur_Timestamp := Gen.Node + Identifier (Shift_Left (T_ms, Sequence_Bits + Node_Bits));
      else
         Gen.Sequence := Gen.Sequence + 1;
      end if;
      Id := Identifier (Gen.Sequence) + Gen.Cur_Timestamp;

      if Util.Log.Loggers.Is_Debug_Enabled (Log) then
         Log.Debug ("Allocated {0} ID {1}", Gen.Name, Id'Image);
      end if;
   end Allocate;

end ADO.Sequences.Snowflake;
