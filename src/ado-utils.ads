-----------------------------------------------------------------------
--  ado-utils -- Utility operations for ADO
--  Copyright (C) 2013, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Containers;
with Ada.Containers.Vectors;

with Util.Beans.Objects;

package ADO.Utils is

   --  Build a bean object from the identifier.
   function To_Object (Id : in ADO.Identifier) return Util.Beans.Objects.Object;

   --  Build the identifier from the bean object.
   function To_Identifier (Value : in Util.Beans.Objects.Object) return ADO.Identifier;

   --  Compute the hash of the identifier.
   function Hash (Key : in ADO.Identifier) return Ada.Containers.Hash_Type;

   package Identifier_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => ADO.Identifier,
                                  "="          => "=");

   subtype Identifier_Vector is Identifier_Vectors.Vector;
   subtype Identifier_Cursor is Identifier_Vectors.Cursor;

   --  Return the identifier list as a comma separated list of identifiers.
   function To_Parameter_List (List : in Identifier_Vector) return String;

end ADO.Utils;
