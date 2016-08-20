-----------------------------------------------------------------------
--  ado-utils-serialize -- Utility operations for JSON/XML serialization
--  Copyright (C) 2016 Stephane Carrez
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

package body ADO.Utils.Serialize is

   --  ------------------------------
   --  Write the entity to the serialization stream (JSON/XML).
   --  ------------------------------
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Identifier) is
   begin
      Stream.Write_Long_Entity (Name, Long_Long_Integer (Value));
   end Write_Entity;

   --  ------------------------------
   --  Write the entity to the serialization stream (JSON/XML).
   --  ------------------------------
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Objects.Object_Key) is
   begin
      case Value.Of_Type is
         when ADO.Objects.KEY_INTEGER =>
            Write_Entity (Stream, Name, ADO.Objects.Get_Value (Value));

         when ADO.Objects.KEY_STRING =>
            Stream.Write_Entity (Name, ADO.Objects.Get_Value (Value));

      end case;
   end Write_Entity;

end ADO.Utils.Serialize;
