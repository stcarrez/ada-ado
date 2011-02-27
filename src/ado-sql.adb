-----------------------------------------------------------------------
--  ADO SQL -- Basic SQL Generation
--  Copyright (C) 2010 Stephane Carrez
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

--  Utilities for creating SQL queries and statements.
package body ADO.SQL is

   function Escape (Str : in Unbounded_String) return String is
   begin
      return Escape (To_String (Str));
   end Escape;

   function Escape (Str : in String) return String is
      S : String (1 .. 2 + Str'Length * 2);
      J : Positive := S'First;
   begin
      if Str = "" then
         return "NULL";
      end if;

      S (J) := ''';

      for K in Str'Range loop
         if Str (K) = ''' then
            J := J + 1;
            S (J) := ''';
         end if;
         J := J + 1;
         S (J) := Str (K);
      end loop;

      J := J + 1;
      S (J) := ''';

      return S (1 .. J);
   end Escape;


   --  --------------------
   --  SQL Dialect
   --  --------------------

   --  --------------------
   --  Check if the string is a reserved keyword.
   --  --------------------
   function Is_Reserved (D    : Dialect;
                         Name : String) return Boolean is
   begin
      return D.Keywords.Contains (Name'Unrestricted_Access);
   end Is_Reserved;

   --  --------------------
   --  Add a set of keywords to be escaped.
   --  --------------------
   procedure Add_Keywords (D        : in out Dialect;
                           Keywords : in Keyword_Array) is
   begin
      for I in Keywords'Range loop
         D.Keywords.Insert (Keywords (I));
      end loop;
   end Add_Keywords;

   --  --------------------
   --  Get the quote character to escape an identifier.
   --  --------------------
   function Get_Identifier_Quote (D : in Dialect) return Character is
      pragma Unreferenced (D);
   begin
      return '`';
   end Get_Identifier_Quote;

   --  --------------------
   --  Buffer
   --  --------------------

   --  --------------------
   --  Clear the SQL buffer.
   --  --------------------
   procedure Clear (Target : in out Buffer) is
   begin
      Target.Buf := To_Unbounded_String ("");
   end Clear;

   --  --------------------
   --  Append an SQL extract into the buffer.
   --  --------------------
   procedure Append (Target : in out Buffer;
                     SQL    : in String) is
   begin
      Append (Target.Buf, SQL);
   end Append;

   --  --------------------
   --  Append a name in the buffer and escape that
   --  name if this is a reserved keyword.
   --  --------------------
   procedure Append_Name (Target : in out Buffer;
                          Name   : in String) is
   begin
      if Target.Dialect /= null and then Target.Dialect.Is_Reserved (Name) then
         declare
            Quote : constant Character := Target.Dialect.Get_Identifier_Quote;
         begin
            Append (Target.Buf, Quote);
            Append (Target.Buf, Name);
            Append (Target.Buf, Quote);
         end;
      else
         Append (Target.Buf, Name);
      end if;
   end Append_Name;

   --  --------------------
   --  Append a string value in the buffer and
   --  escape any special character if necessary.
   --  --------------------
   procedure Append_Value (Target : in out Buffer;
                           Value  : in String) is
   begin
      Append (Target.Buf, Value);
   end Append_Value;

   --  --------------------
   --  Append a string value in the buffer and
   --  escape any special character if necessary.
   --  --------------------
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Unbounded_String) is
   begin
      Append (Target.Buf, Value);
   end Append_Value;

   --  --------------------
   --  Append the integer value in the buffer.
   --  --------------------
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Long_Integer) is
      S : constant String := Long_Integer'Image (Value);
   begin
      Append (Target.Buf, S (S'First + 1 .. S'Last));
   end Append_Value;

   --  --------------------
   --  Append the integer value in the buffer.
   --  --------------------
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Integer) is
      S : constant String := Integer'Image (Value);
   begin
      Append (Target.Buf, S (S'First + 1 .. S'Last));
   end Append_Value;

   --  --------------------
   --  Append the identifier value in the buffer.
   --  --------------------
   procedure Append_Value (Target : in out Buffer;
                           Value  : in Identifier) is
      S : constant String := Identifier'Image (Value);
   begin
      Append (Target.Buf, S (S'First + 1 .. S'Last));
   end Append_Value;

   --  --------------------
   --  Get the SQL string that was accumulated in the buffer.
   --  --------------------
   function To_String (From : in Buffer) return String is
   begin
      return To_String (From.Buf);
   end To_String;

   --  --------------------
   --  Get the SQL dialect description object.
   --  --------------------
   function Get_Dialect (From : in Buffer) return Dialect_Access is
   begin
      return From.Dialect;
   end Get_Dialect;

   --  --------------------
   --  Get the SQL dialect description object.
   --  --------------------
   procedure Set_Dialect (Target : in out Buffer;
                          D      : in Dialect_Access) is
   begin
      Target.Dialect := D;
   end Set_Dialect;

   procedure Set_Filter (Target : in out Query;
                         Filter : in String) is
   begin
      Target.Filter.Buf := To_Unbounded_String (Filter);
   end Set_Filter;

   function Get_Filter (Source : in Query) return String is
   begin
      if Source.Filter.Buf = Null_Unbounded_String then
         return "";
      else
         return To_String (Source.Filter.Buf);
      end if;
   end Get_Filter;

   function Has_Filter (Source : in Query) return Boolean is
   begin
      return Source.Filter.Buf /= Null_Unbounded_String
        and Length (Source.Filter.Buf) > 0;
   end Has_Filter;

   --  --------------------
   --  Set the parameters from another parameter list.
   --  If the parameter list is a query object, also copy the filter part.
   --  --------------------
   procedure Set_Parameters (Params : in out Query;
                             From   : in ADO.Parameters.Abstract_List'Class) is
   begin
      ADO.Parameters.List (Params).Set_Parameters (From);
      if From in Query'Class then
         declare
            L : constant Query'Class := Query'Class (From);
         begin
            Params.Filter := L.Filter;
         end;
      end if;
   end Set_Parameters;

   --  --------------------
   --  Expand the parameters into the query and return the expanded SQL query.
   --  --------------------
   function Expand (Source : in Query) return String is
   begin
      return ADO.Parameters.Abstract_List (Source).Expand (To_String (Source.SQL.Buf));
   end Expand;

   procedure Add_Field (Update : in out Update_Query'Class;
                        Name   : in String) is
   begin
      Update.Pos := Update.Pos + 1;
      if Update.Pos > 1 then
         Append (Target => Update.Set_Fields, SQL => ",");
         Append (Target => Update.Fields, SQL =>",");
      end if;
      Append_Name (Target => Update.Set_Fields, Name => Name);
      if Update.Is_Update_Stmt then
         Append (Target => Update.Set_Fields, SQL => " = ?");
      end if;
      Append (Target => Update.Fields, SQL => "?");
   end Add_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Boolean) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Param (Position => Update.Pos, Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Integer) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Param (Position => Update.Pos, Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Long_Integer) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Param (Position => Update.Pos, Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Identifier) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Add_Param (Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Add_Param (Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in String) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Param (Position => Update.Pos, Value => Value);
   end Save_Field;

   --  --------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  --------------------
   procedure Save_Field (Update : in out Update_Query;
                         Name   : in String;
                         Value  : in Unbounded_String) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Param (Position => Update.Pos, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to NULL.
   --  ------------------------------
   procedure Save_Null_Field (Update : in out Update_Query;
                              Name   : in String) is
   begin
      Update.Add_Field (Name => Name);
      Update_Query'Class (Update).Bind_Null_Param (Position => Update.Pos);
   end Save_Null_Field;

   --  --------------------
   --  Check if the update/insert query has some fields to update.
   --  --------------------
   function Has_Save_Fields (Update : in Update_Query) return Boolean is
   begin
      return Update.Pos > 0;
   end Has_Save_Fields;

   procedure Set_Insert_Mode (Update : in out Update_Query) is
   begin
      Update.Is_Update_Stmt := False;
   end Set_Insert_Mode;

   procedure Append_Fields (Update : in out Update_Query;
                            Mode   : in Boolean := False) is
   begin
      if Mode then
         Append (Target => Update.SQL, SQL => To_String (Update.Fields.Buf));
      else
         Append (Target => Update.SQL, SQL => To_String (Update.Set_Fields.Buf));
      end if;
   end Append_Fields;

end ADO.SQL;
