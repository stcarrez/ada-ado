-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2012, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Directories;
with Ada.Streams.Stream_IO;

with Util.Streams.Files;
with Util.Refs;
package body ADO is

   use Util.Refs;
   use Ada.Streams;

   --  ------------------------------
   --  Create a blob initialized with the content from the file whose path is <b>Path</b>.
   --  Raises an IO exception if the file does not exist.
   --  ------------------------------
   function Create_Blob (Path : in String) return Blob_Ref is
      Size : constant Stream_Element_Offset := Stream_Element_Offset (Ada.Directories.Size (Path));
      File : Util.Streams.Files.File_Stream;
      Last : Stream_Element_Offset;
   begin
      File.Open (Name => Path, Mode => Ada.Streams.Stream_IO.In_File);
      declare
         B    : constant Blob_Access := new Blob '(Ref_Entity with
                                                   Len    => Size,
                                                   others => <>);
      begin
         File.Read (Into => B.Data, Last => Last);
         File.Close;
         return Blob_References.Create (B);
      end;
   end Create_Blob;

   --  ------------------------------
   --  Return True if the two nullable times are identical (both null or both same time).
   --  ------------------------------
   function "=" (Left, Right : in Nullable_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      if Left.Is_Null /= Right.Is_Null then
         return False;
      elsif Left.Is_Null then
         return True;
      else
         return Left.Value = Right.Value;
      end if;
   end "=";

end ADO;
