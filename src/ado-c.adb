-----------------------------------------------------------------------
--  ado-c -- Support for driver implementation
--  Copyright (C) 2009, 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   overriding
   procedure Finalize (S : in out String_Ptr) is
   begin
      Strings.Free (S.Ptr);
   end Finalize;

end ADO.C;
