-----------------------------------------------------------------------
--  ADO Dialects -- Driver support for basic SQL Generation
--  Copyright (C) 2010, 2011, 2012, 2015, 2018 Stephane Carrez
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

package body ADO.Drivers.Dialects is

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
   --  Append the item in the buffer escaping some characters if necessary
   --  ------------------------------
   procedure Escape_Sql (D      : in Dialect;
                         Buffer : in out Unbounded_String;
                         Item   : in ADO.Blob_Ref) is
      pragma Unreferenced (D);

      C    : Ada.Streams.Stream_Element;
      Blob : constant ADO.Blob_Access := Item.Value;
   begin
      Append (Buffer, ''');
      for I in Blob.Data'Range loop
         C := Blob.Data (I);
         case C is
            when Character'Pos (ASCII.NUL) =>
               Append (Buffer, '\');
               Append (Buffer, '0');

            when Character'Pos (ASCII.CR) =>
               Append (Buffer, '\');
               Append (Buffer, 'r');

            when Character'Pos (ASCII.LF) =>
               Append (Buffer, '\');
               Append (Buffer, 'n');

            when Character'Pos ('\') | Character'Pos (''') | Character'Pos ('"') =>
               Append (Buffer, '\');
               Append (Buffer, Character'Val (C));

            when others =>
               Append (Buffer, Character'Val (C));
         end case;
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

end ADO.Drivers.Dialects;
