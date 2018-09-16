-----------------------------------------------------------------------
--  ado-utils-streams -- IO stream utilities
--  Copyright (C) 2018 Stephane Carrez
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
with Ada.Streams;

with Util.Streams;
with Util.Streams.Buffered;

package ADO.Utils.Streams is

   type Blob_Input_Stream is limited new Util.Streams.Input_Stream with private;

   --  Initialize the blob stream to read the content of the blob.
   procedure Initialize (Stream : in out Blob_Input_Stream;
                         Blob   : in ADO.Blob_Ref);

   --  Read into the buffer as many bytes as possible and return in
   --  <b>last</b> the position of the last byte read.
   overriding
   procedure Read (Stream : in out Blob_Input_Stream;
                   Into   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   type Blob_Output_Stream is limited new
     Util.Streams.Buffered.Output_Buffer_Stream with null record;

   function Get_Blob (Stream : in Blob_Output_Stream) return Blob_Ref;

private

   type Blob_Input_Stream is limited new Util.Streams.Input_Stream with record
      Data : Blob_Ref;
      Pos  : Ada.Streams.Stream_Element_Offset := 1;
   end record;

end ADO.Utils.Streams;
