-----------------------------------------------------------------------
--  ado-sequences-hilo-- HiLo Database sequence generator
--  Copyright (C) 2009 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings;
with Util.Log;
with Util.Log.Loggers;
with ADO.Sessions;
with ADO.Model;
with ADO.Objects;
with ADO.SQL;

package body ADO.Sequences.Hilo is

   use Util.Log;
   use ADO.Sessions;
   use ADO.Model;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Sequences.Hilo");

   type Hilo_Generator_Access is access all Hilo_Generator'Class;

   --  ------------------------------
   --  Create a high low sequence generator
   --  ------------------------------
   function Create_HiLo_Generator
     (Sess_Factory : in Session_Factory_Access;
      Name         : in String)
      return Generator_Access is
      Result : constant Hilo_Generator_Access
        := new Hilo_Generator '(Ada.Finalization.Limited_Controlled with
                                Name_Length => Name'Length,
                                Factory => Sess_Factory,
                                Name => Name,
                                others => <>);
   begin
      return Result.all'Access;
   end Create_HiLo_Generator;

   --  ------------------------------
   --  Allocate an identifier using the generator.
   --  The generator allocates blocks of sequences by using a sequence
   --  table stored in the database.  One database access is necessary
   --  every N allocations.
   --  ------------------------------
   overriding
   procedure Allocate (Gen     : in out Hilo_Generator;
                       Session : in out ADO.Sessions.Master_Session'Class;
                       Id      : in out Identifier) is
   begin
      --  Get a new sequence range
      if Gen.Next_Id >= Gen.Last_Id then
         Allocate_Sequence (Gen, Session);
      end if;

      Id := Gen.Next_Id;
      Gen.Next_Id := Gen.Next_Id + 1;
   end Allocate;

   --  ------------------------------
   --  Allocate a new sequence block.
   --  ------------------------------
   procedure Allocate_Sequence (Gen     : in out Hilo_Generator;
                                Session : in out ADO.Sessions.Master_Session'Class) is
      Value     : Identifier;
      Seq_Block : Sequence_Ref;
      DB        : Master_Session'Class
        := (if Gen.Use_New_Session then Gen.Get_Session else Session);
   begin
      for Retry in 1 .. 10 loop
         --  Allocate a new sequence within a transaction.
         declare
            Query : ADO.SQL.Query;
            Found : Boolean;
         begin
            Log.Info ("Allocate sequence range for {0}", Gen.Name);

            DB.Begin_Transaction;
            Query.Set_Filter ("name = ?");
            Query.Bind_Param (Position => 1, Value => Gen.Name);
            Seq_Block.Find (Session => DB, Query => Query, Found => Found);

            begin
               if Found then
                  Value := Seq_Block.Get_Value;
                  Seq_Block.Set_Value (Value + Seq_Block.Get_Block_Size);
                  Seq_Block.Save (DB);
               else
                  Value := 1;
                  Seq_Block.Set_Name (Gen.Name);
                  Seq_Block.Set_Block_Size (Gen.Block_Size);
                  Seq_Block.Set_Value (Value + Seq_Block.Get_Block_Size);
                  Seq_Block.Create (DB);
               end if;
               DB.Commit;

               Gen.Next_Id := Value;
               Gen.Last_Id := Seq_Block.Get_Value;
               return;

            exception
               when ADO.Objects.LAZY_LOCK =>
                  Log.Info ("Sequence table modified, retrying {0}/100",
                            Util.Strings.Image (Retry));
                  DB.Rollback;
                  delay 0.01 * Retry;
            end;

         exception
            when E : others =>
               Log.Error ("Cannot allocate sequence range", E);
               raise;
         end;
      end loop;
      Log.Error ("Cannot allocate sequence range for {0}", Gen.Name);
      raise Allocate_Error with "Cannot allocate unique identifier";
   end Allocate_Sequence;

end ADO.Sequences.Hilo;
