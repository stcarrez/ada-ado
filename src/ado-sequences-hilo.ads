-----------------------------------------------------------------------
--  ADO Sequences Hilo-- HiLo Database sequence generator
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

--  The HiLo sequence generator.  This sequence generator uses a database table
--  <b>sequence</b> to allocate blocks of identifiers for a given sequence name.
--  The sequence table contains one row for each sequence.  It keeps track of
--  the next available sequence identifier (in the <b>value</b> column).
--
--  To allocate a sequence block, the HiLo generator obtains the next available
--  sequence identified and updates it by adding the sequence block size.  The
--  HiLo sequence generator will allocate the identifiers until the block is
--  full after which a new block will be allocated.

package ADO.Sequences.Hilo is

   --  ------------------------------
   --  High Low sequence generator
   --  ------------------------------
   type HiLoGenerator is new Generator with private;

   DEFAULT_BLOCK_SIZE : constant Identifier := 100;

   --  Allocate an identifier using the generator.
   --  The generator allocates blocks of sequences by using a sequence
   --  table stored in the database.  One database access is necessary
   --  every N allocations.
   overriding
   procedure Allocate (Gen : in out HiLoGenerator;
                       Id  : out Identifier);

   --  Allocate a new sequence block.
   procedure Allocate_Sequence (Gen : in out HiLoGenerator);

   function Create_HiLo_Generator
     (Sess_Factory : access ADO.Sessions.Factory.Session_Factory'Class)
      return Generator_Access;

private

   type HiLoGenerator is new Generator with record
      Last_Id    : Identifier := NO_IDENTIFIER;
      Next_Id    : Identifier := NO_IDENTIFIER;
      Block_Size : Identifier := DEFAULT_BLOCK_SIZE;
   end record;

end ADO.Sequences.Hilo;
