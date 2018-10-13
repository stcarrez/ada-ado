-----------------------------------------------------------------------
--  ado-c -- Support for driver implementation
--  Copyright (C) 2009, 2010, 2018 Stephane Carrez
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

package body ADO.C is

   use type Interfaces.C.Strings.chars_ptr;

   --  ------------------------------
   --  Convert a string to a C string.
   --  ------------------------------
   function To_String_Ptr (S : String) return String_Ptr is
   begin
      return Result : String_Ptr do
         Result.Ptr := Strings.New_String (S);
      end return;
   end To_String_Ptr;

   --  ------------------------------
   --  Get the C string pointer.
   --  ------------------------------
   function To_C (S : String_Ptr) return Interfaces.C.Strings.chars_ptr is
   begin
      return S.Ptr;
   end To_C;

   --  ------------------------------
   --  Set the string
   --  ------------------------------
   procedure Set_String (S     : in out String_Ptr;
                         Value : in String) is
   begin
      if S.Ptr /= Interfaces.C.Strings.Null_Ptr then
         Strings.Free (S.Ptr);
      end if;
      S.Ptr := Strings.New_String (Value);
   end Set_String;

   --  ------------------------------
   --  Reclaim the storage held by the C string.
   --  ------------------------------
   procedure Finalize (S : in out String_Ptr) is
   begin
      Strings.Free (S.Ptr);
   end Finalize;

end ADO.C;
