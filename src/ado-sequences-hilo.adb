-----------------------------------------------------------------------
--  ADO Sequences -- Database sequence generator
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

with Util.Log;
with Util.Log.Loggers;
with ADO.Sessions;
with ADO.Model;
with ADO.Objects;

package body ADO.Sequences.Hilo is

   use Util.Log;
   use ADO.Sessions;
   use Sequence_Maps;
   use ADO.Model;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Sequences.Hilo");

   type HiLoGenerator_Access is access all HiLoGenerator'Class;

   --  ------------------------------
   --  Create a high low sequence generator
   --  ------------------------------
   function Create_HiLo_Generator
     (Sess_Factory : access ADO.Sessions.Factory.Session_Factory'Class)
      return Generator_Access is
      Result : constant HiLoGenerator_Access := new HiLoGenerator;
   begin
      Result.Factory := Sess_Factory;
      return Result.all'Access;
   end Create_HiLo_Generator;

   --  ------------------------------
   --  Allocate an identifier using the generator.
   --  The generator allocates blocks of sequences by using a sequence
   --  table stored in the database.  One database access is necessary
   --  every N allocations.
   --  ------------------------------
   procedure Allocate (Gen : in out HiLoGenerator;
                       Id  : out Identifier) is
   begin
      --  Get a new sequence range
      if Gen.Next_Id >= Gen.Last_Id then
         Allocate_Sequence (Gen);
      end if;

      Id := Gen.Next_Id;
      Gen.Next_Id := Gen.Next_Id + 1;
   end Allocate;

   --  ------------------------------
   --  Allocate a new sequence block.
   --  ------------------------------
   procedure Allocate_Sequence (Gen : in out HiLoGenerator) is
      Name      : constant String := Get_Sequence_Name (Gen);
      Value     : Identifier;
      Seq_Block : Sequence_Ref;
      DB        : Master_Session'Class := Gen.Get_Session;
   begin
      loop
         --  Allocate a new sequence within a transaction.
         declare
            Params : Parameter_List;
            Found  : Boolean;
         begin
            Log.Info ("Allocate sequence range for {0}", Name);

            DB.Begin_Transaction;
            Params.Set_Filter ("name = ?");
            Params.Bind_Param (Position => 1, Value => Name);
            Seq_Block.Find (Database => DB, Parameters => Params, Found => Found);

            begin
               if Found then
                  Value := Seq_Block.Get_Value;
                  Seq_Block.Set_Value (Value + Seq_Block.Get_Block_Size);
                  Seq_Block.Save (DB);
               else
                  Value := 1;
                  Seq_Block.Set_Name (Name);
                  Seq_Block.Set_Block_Size (Gen.Block_Size);
                  Seq_Block.Set_Value (Value + Seq_Block.Get_Block_Size);
                  Seq_Block.Save (DB);
               end if;
               DB.Commit;

               Gen.Next_Id := Value;
               Gen.Last_Id := Seq_Block.Get_Value;
               return;

            exception
               when ADO.Objects.LAZY_LOCK =>
                  Log.Info ("Sequence table modified, retrying");
                  DB.Rollback;
            end;

         exception
            when E : others =>
               Log.Error ("Cannot allocate sequence range", E);
               raise;
         end;
      end loop;
   end Allocate_Sequence;

end ADO.Sequences.Hilo;
