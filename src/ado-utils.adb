-----------------------------------------------------------------------
--  ado-utils -- Utility operations for ADO
--  Copyright (C) 2013 Stephane Carrez
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

package body ADO.Utils is

   --  ------------------------------
   --  Build a bean object from the identifier.
   --  ------------------------------
   function To_Object (Id : in ADO.Identifier) return Util.Beans.Objects.Object is
   begin
      if Id = ADO.NO_IDENTIFIER then
         return Util.Beans.Objects.Null_Object;
      else
         return Util.Beans.Objects.To_Object (Long_Long_Integer (Id));
      end if;
   end To_Object;

   --  ------------------------------
   --  Build the identifier from the bean object.
   --  ------------------------------
   function To_Identifier (Value : in Util.Beans.Objects.Object) return ADO.Identifier is
   begin
      if Util.Beans.Objects.Is_Null (Value) then
         return ADO.NO_IDENTIFIER;
      else
         return ADO.Identifier (Util.Beans.Objects.To_Long_Long_Integer (Value));
      end if;
   end To_Identifier;

   --  ------------------------------
   --  Compute the hash of the identifier.
   --  ------------------------------
   function Hash (Key : in ADO.Identifier) return Ada.Containers.Hash_Type is
      use Ada.Containers;
   begin

      if Key < 0 then
         return Hash_Type (-Key);
      else
         return Hash_Type (Key);
      end if;
   end Hash;

   --  ------------------------------
   --  Return the identifier list as a comma separated list of identifiers.
   --  ------------------------------
   function To_Parameter_List (List : in Identifier_Vector) return String is
      use Identifier_Vectors;

      Result     : Ada.Strings.Unbounded.Unbounded_String;
      Pos        : Identifier_Cursor := List.First;
      Need_Comma : Boolean := False;
   begin
      while Identifier_Vectors.Has_Element (Pos) loop
         if Need_Comma then
            Ada.Strings.Unbounded.Append (Result, ",");
         end if;
         Ada.Strings.Unbounded.Append (Result, ADO.Identifier'Image (Element (Pos)));
         Need_Comma := True;
         Next (Pos);
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end To_Parameter_List;

end ADO.Utils;
