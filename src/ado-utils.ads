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
with Ada.Containers;

with Util.Beans.Objects;
package ADO.Utils is

   --  Build a bean object from the identifier.
   function To_Object (Id : in ADO.Identifier) return Util.Beans.Objects.Object;

   --  Build the identifier from the bean object.
   function To_Identifier (Value : in Util.Beans.Objects.Object) return ADO.Identifier;

   --  Compute the hash of the identifier.
   function Hash (Key : in ADO.Identifier) return Ada.Containers.Hash_Type;

end ADO.Utils;
