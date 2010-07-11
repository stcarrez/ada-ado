-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2009, 2010 Stephane Carrez
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
package ADO is

   subtype Int8 is Interfaces.C.signed_char;

   subtype Int16 is Interfaces.C.short;

   subtype Int32 is Interfaces.C.int;

   type Int64 is range -2**63 .. 2**63 - 1;
   for Int64'Size use 64;

   type Unsigned64 is mod 2**64;
   for Unsigned64'Size use 64;

end ADO;
