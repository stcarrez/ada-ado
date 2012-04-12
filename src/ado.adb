-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2012 Stephane Carrez
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

package body ADO is

   use Util.Refs;
   use Ada.Streams;

   --  ------------------------------
   --  Create a blob with an allocated buffer of <b>Size</b> bytes.
   --  ------------------------------
   function Create_Blob (Size : in Natural) return Blob_Ref is
      B :  constant Blob_Access := new Blob '(Ref_Entity with
                                              Len    => Stream_Element_Offset (Size),
                                              others => <>);
   begin
      return Blob_References.Create (B);
   end Create_Blob;

   --  ------------------------------
   --  Create a blob initialized with the given data buffer.
   --  ------------------------------
   function Create_Blob (Data : in Ada.Streams.Stream_Element_Array) return Blob_Ref is
      B   :  constant Blob_Access := new Blob '(Ref_Entity with
                                                Len    => Data'Length,
                                                Data   => Data);
   begin
      return Blob_References.Create (B);
   end Create_Blob;


end ADO;
