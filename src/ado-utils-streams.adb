-----------------------------------------------------------------------
--  ado-utils-streams -- IO stream utilities
--  Copyright (C) 2018, 2019 Stephane Carrez
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

package body ADO.Utils.Streams is

   use type Ada.Streams.Stream_Element_Offset;
   subtype Offset is Ada.Streams.Stream_Element_Offset;

   --  ------------------------------
   --  Initialize the blob stream to read the content of the blob.
   --  ------------------------------
   procedure Initialize (Stream : in out Blob_Input_Stream;
                         Blob   : in ADO.Blob_Ref) is
   begin
      Stream.Data := Blob;
      Stream.Pos  := 1;
   end Initialize;

   --  ------------------------------
   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   --  ------------------------------
   overriding
   procedure Read (Stream : in out Blob_Input_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Stream.Data.Is_Null then
         Last := Into'First - 1;
      else
         declare
            Blob  : constant Blob_Accessor := Stream.Data.Value;
            Avail : Offset := Blob.Data'Last - Stream.Pos + 1;
         begin
            if Avail > Into'Length then
               Avail := Into'Length;
            end if;
            Last := Into'First + Avail - 1;
            if Avail > 0 then
               Into (Into'First .. Last) := Blob.Data (Stream.Pos .. Stream.Pos + Avail - 1);
               Stream.Pos := Stream.Pos + Avail;
            end if;
         end;
      end if;
   end Read;

   function Get_Blob (Stream : in Blob_Output_Stream) return Blob_Ref is
      Size   : constant Offset := Offset (Stream.Get_Size);
      Buffer : constant Util.Streams.Buffered.Buffer_Access := Stream.Get_Buffer;
   begin
      return Create_Blob (Data => Buffer (Buffer'First .. Buffer'First + Size - 1));
   end Get_Blob;

end ADO.Utils.Streams;
