-----------------------------------------------------------------------
--  ADO Dialects -- Driver support for basic SQL Generation
--  Copyright (C) 2010, 2011, 2012, 2015, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ADO.Dialects is

   --  ------------------------------
   --  Append the item in the buffer escaping some characters if necessary.
   --  The default implementation only escapes the single quote ' by doubling them.
   --  ------------------------------
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in String) is
      pragma Unreferenced (D);

      C  : Character;
   begin
      Append (Buffer, ''');
      for I in Item'Range loop
         C := Item (I);
         if C = ''' then
            Append (Buffer, ''');
         end if;
         Append (Buffer, C);
      end loop;
      Append (Buffer, ''');
   end Escape_Sql;

   --  ------------------------------
   --  Append the boolean item in the buffer.
   --  ------------------------------
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in Boolean) is
      pragma Unreferenced (D);
   begin
      Append (Buffer, (if Item then '1' else '0'));
   end Escape_Sql;

end ADO.Dialects;
