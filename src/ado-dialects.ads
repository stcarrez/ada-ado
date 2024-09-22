-----------------------------------------------------------------------
--  ado-dialects -- Driver support for basic SQL Generation
--  Copyright (C) 2010, 2011, 2012, 2015, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;

--  The `ADO.Dialects` package controls the database specific SQL dialects.
package ADO.Dialects is

   use Ada.Strings.Unbounded;

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
   type Dialect is abstract tagged private;
   type Dialect_Access is access all Dialect'Class;

   --  Check if the string is a reserved keyword.
   function Is_Reserved (D    : Dialect;
                         Name : String) return Boolean is abstract;

   --  Get the quote character to escape an identifier.
   function Get_Identifier_Quote (D : in Dialect) return Character is abstract;

   --  Append the item in the buffer escaping some characters if necessary.
   --  The default implementation only escapes the single quote ' by doubling them.
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in String);

   --  Append the item in the buffer escaping some characters if necessary
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref) is abstract;

   --  Append the boolean item in the buffer.
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in Boolean);

private

   type Dialect is abstract tagged null record;

end ADO.Dialects;
