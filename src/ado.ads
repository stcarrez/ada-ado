-----------------------------------------------------------------------
--  ADO Databases -- Database Objects
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Calendar;
package ADO is

   type Int64 is range -2**63 .. 2**63 - 1;
   for Int64'Size use 64;

   type Unsigned64 is mod 2**64;
   for Unsigned64'Size use 64;

   --  ------------------------------
   --  Database Identifier
   --  ------------------------------
   --
   type Identifier is new Integer;

   NO_IDENTIFIER : constant Identifier := -1;

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

end ADO;
