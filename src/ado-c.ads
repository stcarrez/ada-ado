-----------------------------------------------------------------------
--  ado-c -- Support for driver implementation
--  Copyright (C) 2009, 2010, 2011, 2012, 2018 Stephane Carrez
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

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;
package ADO.C is

   type String_Ptr is limited private;

   --  Convert a string to a C string.
   function To_String_Ptr (S : String) return String_Ptr;

   --  Get the C string pointer.
   function To_C (S : String_Ptr) return Interfaces.C.Strings.chars_ptr;

   --  Set the string
   procedure Set_String (S     : in out String_Ptr;
                         Value : in String);

   function To_chars_ptr is
      new Ada.Unchecked_Conversion (System.Address, Interfaces.C.Strings.chars_ptr);

private

   use Interfaces.C;

   type String_Ptr is new Ada.Finalization.Limited_Controlled with record
      Ptr : Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
   end record;

   --  Reclaim the storage held by the C string.
   procedure Finalize (S : in out String_Ptr);

end ADO.C;
