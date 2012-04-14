-----------------------------------------------------------------------
--  ADO Dialects -- Driver support for basic SQL Generation
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Util.Strings;

--  The <b>ADO.Drivers.Dialects</b> package controls the database specific SQL dialects.
package ADO.Drivers.Dialects is

   type Keyword_Array is array (Natural range <>) of Util.Strings.Name_Access;

   --  --------------------
   --  SQL Dialect
   --  --------------------
   --  The <b>Dialect</b> defines the specific characteristics that must be
   --  taken into account when building the SQL statement.  This includes:
   --  <ul>
   --    <li>The definition of reserved keywords that must be escaped</li>
   --    <li>How to escape those keywords</li>
   --    <li>How to escape special characters</li>
   --  </ul>
   type Dialect is tagged private;
   type Dialect_Access is access all Dialect'Class;

   --  Check if the string is a reserved keyword.
   function Is_Reserved (D    : Dialect;
                         Name : String) return Boolean;

   --  Add a set of keywords to be escaped.
   procedure Add_Keywords (D        : in out Dialect;
                           Keywords : in Keyword_Array);

   --  Get the quote character to escape an identifier.
   function Get_Identifier_Quote (D : in Dialect) return Character;

   --  Append the item in the buffer escaping some characters if necessary.
   --  The default implementation only escapes the single quote ' by doubling them.
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in String);

   --  Append the item in the buffer escaping some characters if necessary
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref);

private

   type Dialect is tagged record
      Keywords : Util.Strings.String_Set.Set;
   end record;

end ADO.Drivers.Dialects;
