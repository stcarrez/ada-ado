-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2009, 2010, 2011, 2012, 2015 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Calendar;

with Util.Refs;
package ADO is

   type Int64 is range -2**63 .. 2**63 - 1;
   for Int64'Size use 64;

   type Unsigned64 is mod 2**64;
   for Unsigned64'Size use 64;

   DEFAULT_TIME : constant Ada.Calendar.Time;

   --  ------------------------------
   --  Database Identifier
   --  ------------------------------
   --
   type Identifier is range -2**47 .. 2**47 - 1;

   NO_IDENTIFIER : constant Identifier := -1;

   type Entity_Type is range 0 .. 2**16 - 1;

   NO_ENTITY_TYPE : constant Entity_Type := 0;

   type Object_Id is record
      Id   : Identifier;
      Kind : Entity_Type;
   end record;
   pragma Pack (Object_Id);

   --  ------------------------------
   --  Nullable Types
   --  ------------------------------
   --  Most database allow to store a NULL instead of an actual integer, date or string value.
   --  Unlike Java, there is no easy way to distinguish between a NULL and an actual valid value.
   --  The <b>Nullable_T</b> types provide a way to specify and check whether a value is null
   --  or not.

   --  An integer which can be null.
   type Nullable_Integer is record
      Value   : Integer := 0;
      Is_Null : Boolean := True;
   end record;

   --  A string which can be null.
   type Nullable_String is record
      Value   : Ada.Strings.Unbounded.Unbounded_String;
      Is_Null : Boolean := True;
   end record;

   --  A date which can be null.
   type Nullable_Time is record
      Value   : Ada.Calendar.Time;
      Is_Null : Boolean := True;
   end record;

   type Nullable_Entity_Type is record
      Value   : Entity_Type := 0;
      Is_Null : Boolean := True;
   end record;

   --  ------------------------------
   --  Blob data type
   --  ------------------------------
   --  The <b>Blob</b> type is used to represent database blobs.  The data is stored
   --  in an <b>Ada.Streams.Stream_Element_Array</b> pointed to by the <b>Data</b> member.
   --  The query statement and bind parameter will use a <b>Blob_Ref</b> which represents
   --  a reference to the blob data.  This is intended to minimize data copy.
   type Blob (Len : Ada.Streams.Stream_Element_Offset) is new Util.Refs.Ref_Entity with record
      Data : Ada.Streams.Stream_Element_Array (1 .. Len);
   end record;
   type Blob_Access is access all Blob;

   package Blob_References is new Util.Refs.Indefinite_References (Blob, Blob_Access);
   subtype Blob_Ref is Blob_References.Ref;

   --  Create a blob with an allocated buffer of <b>Size</b> bytes.
   function Create_Blob (Size : in Natural) return Blob_Ref;

   --  Create a blob initialized with the given data buffer.
   function Create_Blob (Data : in Ada.Streams.Stream_Element_Array) return Blob_Ref;

   --  Create a blob initialized with the content from the file whose path is <b>Path</b>.
   --  Raises an IO exception if the file does not exist.
   function Create_Blob (Path : in String) return Blob_Ref;

   --  Return a null blob.
   function Null_Blob return Blob_Ref;

private

   DEFAULT_TIME : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year    => 1901,
                                                                      Month   => 1,
                                                                      Day     => 2,
                                                                      Seconds => 0.0);

end ADO;
