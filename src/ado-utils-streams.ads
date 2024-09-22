-----------------------------------------------------------------------
--  ado-utils-streams -- IO stream utilities
--  Copyright (C) 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
