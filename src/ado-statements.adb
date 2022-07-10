-----------------------------------------------------------------------
--  ado-statements -- Database statements
--  Copyright (C) 2009 - 2022 Stephane Carrez
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

with Ada.Calendar.Formatting;

with Util.Log;
with Util.Log.Loggers;
with System.Storage_Elements;
with Ada.Unchecked_Deallocation;
package body ADO.Statements is

   use System.Storage_Elements;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Statements");

   function Get_Query (Query : Statement) return ADO.SQL.Query_Access is
   begin
      return Query.Query;
   end Get_Query;

   overriding
   procedure Add_Parameter (Query : in out Statement;
                            Param : in ADO.Parameters.Parameter) is
   begin
      Query.Query.Add_Parameter (Param);
   end Add_Parameter;

   overriding
   procedure Set_Parameters (Query : in out Statement;
                             From  : in ADO.Parameters.Abstract_List'Class) is
   begin
      Query.Query.Set_Parameters (From);
   end Set_Parameters;

   --  ------------------------------
   --  Return the number of parameters in the list.
   --  ------------------------------
   overriding
   function Length (Query : in Statement) return Natural is
   begin
      return Query.Query.Length;
   end Length;

   --  ------------------------------
   --  Return the parameter at the given position
   --  ------------------------------
   overriding
   function Element (Query    : in Statement;
                     Position : in Natural) return ADO.Parameters.Parameter is
   begin
      return Query.Query.Element (Position);
   end Element;

   --  ------------------------------
   --  Execute the <b>Process</b> procedure with the given parameter as argument.
   --  ------------------------------
   overriding
   procedure Query_Element (Query    : in Statement;
                            Position : in Natural;
                            Process  : not null access
                              procedure (Element : in ADO.Parameters.Parameter)) is
   begin
      Query.Query.Query_Element (Position, Process);
   end Query_Element;

   --  ------------------------------
   --  Clear the list of parameters.
   --  ------------------------------
   overriding
   procedure Clear (Query : in out Statement) is
   begin
      Query.Query.Clear;
   end Clear;

   procedure Add_Param (Params : in out Statement;
                        Value : in ADO.Objects.Object_Key) is
   begin
      case Value.Of_Type is
         when ADO.Objects.KEY_INTEGER =>
            declare
               V : constant Identifier := Objects.Get_Value (Value);
            begin
               Params.Query.Add_Param (V);
            end;

         when ADO.Objects.KEY_STRING =>
            declare
               V : constant Unbounded_String := Objects.Get_Value (Value);
            begin
               Params.Query.Add_Param (V);
            end;

      end case;
   end Add_Param;

   --  ------------------------------
   --  Add the parameter by using the primary key of the object.
   --  Use null if the object is a null reference.
   --  ------------------------------
   procedure Add_Param (Params : in out Statement;
                        Value  : in ADO.Objects.Object_Ref'Class) is
   begin
      if Value.Is_Null then
         Params.Query.Add_Null_Param;
      else
         Params.Add_Param (Value.Get_Key);
      end if;
   end Add_Param;

   procedure Append (Query : in out Statement; SQL : in String) is
   begin
      ADO.SQL.Append (Target => Query.Query.SQL, SQL => SQL);
   end Append;

   procedure Append (Query : in out Statement; Value : in Integer) is
   begin
      ADO.SQL.Append_Value (Target => Query.Query.SQL, Value => Long_Integer (Value));
   end Append;

   procedure Append (Query : in out Statement; Value : in Long_Integer) is
   begin
      ADO.SQL.Append_Value (Target => Query.Query.SQL, Value => Value);
   end Append;

   procedure Append (Query : in out Statement; SQL : in Unbounded_String) is
   begin
      ADO.SQL.Append_Value (Target => Query.Query.SQL, Value => To_String (SQL));
   end Append;

   procedure Set_Filter (Query  : in out Statement;
                         Filter : in String) is
   begin
      Query.Query.Set_Filter (Filter);
   end Set_Filter;

   --  ------------------------------
   --  Get the filter condition or the empty string
   --  ------------------------------
--     function Get_Filter (Parameters : in Statement) return String is
--     begin
--        return Parameters.Query.Get_Filter;
--     end Get_Filter;

   procedure Execute (Query  : in out Statement;
                      SQL    : in Unbounded_String;
                      Params : in ADO.Parameters.Abstract_List'Class) is
   begin
      null;
   end Execute;

   --  ------------------------------
   --  Append the value to the SQL query string.
   --  ------------------------------
   procedure Append_Escape (Query : in out Statement; Value : in String) is
   begin
      ADO.SQL.Append_Value (Query.Query.SQL, Value);
   end Append_Escape;

   --  ------------------------------
   --  Append the value to the SQL query string.
   --  ------------------------------
   procedure Append_Escape (Query : in out Statement; Value : in Unbounded_String) is
   begin
      ADO.SQL.Append_Value (Query.Query.SQL, To_String (Value));
   end Append_Escape;

   function "+" (Left : chars_ptr; Right : Size_T) return chars_ptr is
   begin
      return To_Chars_Ptr (To_Address (Left) + Storage_Offset (Right));
   end "+";

   --  ------------------------------
   --  Get the query result as an integer
   --  ------------------------------
   function Get_Result_Integer (Query : Query_Statement) return Integer is
   begin
      if not Query_Statement'Class (Query).Has_Elements then
         return 0;
      end if;
      if Query_Statement'Class (Query).Is_Null (0) then
         return 0;
      end if;
      return Query_Statement'Class (Query).Get_Integer (0);
   end Get_Result_Integer;

   --  ------------------------------
   --  Get the query result as a blob
   --  ------------------------------
   function Get_Result_Blob (Query : in Query_Statement) return ADO.Blob_Ref is
   begin
      if not Query_Statement'Class (Query).Has_Elements then
         return Null_Blob;
      end if;
      return Query_Statement'Class (Query).Get_Blob (0);
   end Get_Result_Blob;

   --  ------------------------------
   --  Get an unsigned 64-bit number from a C string terminated by \0
   --  ------------------------------
   function Get_Uint64 (Str : chars_ptr) return unsigned_long is
      C      : Character;
      P      : chars_ptr := Str;
      Result : unsigned_long := 0;
   begin
      loop
         C := P.all;
         if C /= ' ' then
            exit;
         end if;
         P := P + 1;
      end loop;
      while C >= '0' and then C <= '9' loop
         Result := Result * 10 + unsigned_long (Character'Pos (C) - Character'Pos ('0'));
         P := P + 1;
         C := P.all;
      end loop;
      if C /= ASCII.NUL then
         raise Invalid_Type with "Invalid integer value";
      end if;
      return Result;
   end Get_Uint64;

   --  ------------------------------
   --  Get a signed 64-bit number from a C string terminated by \0
   --  ------------------------------
   function Get_Int64 (Str : chars_ptr) return Int64 is
      C : Character;
      P : chars_ptr := Str;
   begin
      if P = null then
         return 0;
      end if;
      loop
         C := P.all;
         if C /= ' ' then
            exit;
         end if;
         P := P + 1;
      end loop;
      if C = '+' then
         P := P + 1;
         return Int64 (Get_Uint64 (P));

      elsif C = '-' then
         P := P + 1;
         return -Int64 (Get_Uint64 (P));

      else
         return Int64 (Get_Uint64 (P));
      end if;
   end Get_Int64;

   --  ------------------------------
   --  Get a double number from a C string terminated by \0
   --  ------------------------------
   function Get_Long_Float (Str : chars_ptr) return Long_Float is
      C : Character;
      P : chars_ptr := Str;
   begin
      if P = null then
         return 0.0;
      end if;
      loop
         C := P.all;
         if C /= ' ' then
            exit;
         end if;
         P := P + 1;
      end loop;
      declare
         S : String (1 .. 100);
      begin
         for I in S'Range loop
            C := P.all;
            S (I) := C;
            if C = ASCII.NUL then
               return Long_Float'Value (S (S'First .. I - 1));
            end if;
            P := P + 1;
         end loop;
         raise Invalid_Type with "Invalid floating point value";
      end;
   end Get_Long_Float;

   --  ------------------------------
   --  Get a time from the C string passed in <b>Value</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Time (Value  : in chars_ptr) return Ada.Calendar.Time is
      use Ada.Calendar;

      Year   : Year_Number  := Year_Number'First;
      Month  : Month_Number := Month_Number'First;
      Day    : Day_Number   := Day_Number'First;
      Hours  : Natural      := 0;
      Mins   : Natural      := 0;
      Secs   : Natural      := 0;
      Dt     : Duration     := 0.0;
      Field  : chars_ptr    := Value;

      function Get_Number (P : in chars_ptr;
                           Nb_Digits : in Positive) return Natural;

      --  ------------------------------
      --  Get a number composed of N digits
      --  ------------------------------
      function Get_Number (P : in chars_ptr;
                           Nb_Digits : in Positive) return Natural is
         Ptr    : chars_ptr := P;
         Result : Natural   := 0;
         C      : Character;
      begin
         for I in 1 .. Nb_Digits loop
            C := Ptr.all;
            if not (C >= '0' and then C <= '9') then
               raise Invalid_Type with "Invalid date format";
            end if;
            Result := Result * 10 + Character'Pos (C) - Character'Pos ('0');
            Ptr := Ptr + 1;
         end loop;
         return Result;
      end Get_Number;

   begin
      if Field /= null then
         declare
            C : Character;
            N : Natural;
         begin
            N := Get_Number (Field, 4);
            if N /= 0 then
               Year := Year_Number (N);
            end if;
            Field := Field + 4;
            C := Field.all;
            if C /= '-' then
               raise Invalid_Type with "Invalid date format";
            end if;
            Field := Field + 1;

            N := Get_Number (Field, 2);
            if N /= 0 then
               Month := Month_Number (N);
            end if;
            Field := Field + 2;
            C := Field.all;
            if C /= '-' then
               raise Invalid_Type with "Invalid date format";
            end if;
            Field := Field + 1;

            N := Get_Number (Field, 2);
            if N /= 0 then
               Day := Day_Number (N);
            end if;
            Field := Field + 2;
            C := Field.all;
            if C /= ASCII.NUL then
               if C /= ' ' then
                  raise Invalid_Type with "Invalid date format";
               end if;
               Field := Field + 1;

               Hours := Get_Number (Field, 2);
               Field := Field + 2;
               C := Field.all;
               if C /= ':' then
                  raise Invalid_Type with "Invalid date format";
               end if;
               Field := Field + 1;

               Mins := Get_Number (Field, 2);
               Field := Field + 2;
               C := Field.all;
               if C /= ':' then
                  raise Invalid_Type with "Invalid date format";
               end if;
               Field := Field + 1;

               Secs := Get_Number (Field, 2);
               Field := Field + 2;
               C := Field.all;
               if C /= '.' and then C /= ASCII.NUL then
                  raise Invalid_Type with "Invalid date format";
               end if;
               Dt := Duration (Hours * 3600) + Duration (Mins * 60) + Duration (Secs);
            end if;
         end;
      end if;

      return Ada.Calendar.Formatting.Time_Of (Year, Month, Day, Dt, False, 0);
   end Get_Time;

   --  ------------------------------
   --  Create a blob initialized with the given data buffer pointed to by <b>Data</b>
   --  and which contains <b>Size</b> bytes.
   --  ------------------------------
   function Get_Blob (Data : in chars_ptr;
                      Size : in Natural) return Blob_Ref is
      use Util.Refs;
      use Ada.Streams;

      B :  constant Blob_Access := new Blob '(Ref_Entity with
                                              Len    => Stream_Element_Offset (Size),
                                              others => <>);
      P : chars_ptr := Data;
   begin
      for I in 1 .. Stream_Element_Offset (Size) loop
         B.Data (I) := Character'Pos (P.all);
         P := P + 1;
      end loop;
      return Blob_References.Create (B);
   end Get_Blob;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Query : in out Query_Statement) is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      Query.Proxy.Execute;
   end Execute;

   --  ------------------------------
   --  Get the number of rows returned by the query
   --  ------------------------------
   function Get_Row_Count (Query : in Query_Statement) return Natural is
   begin
      if Query.Proxy = null then
         return 0;
      else
         return Query.Proxy.Get_Row_Count;
      end if;
   end Get_Row_Count;

   --  ------------------------------
   --  Returns True if there is more data (row) to fetch
   --  ------------------------------
   function Has_Elements (Query : in Query_Statement) return Boolean is
   begin
      if Query.Proxy = null then
         return False;
      else
         return Query.Proxy.Has_Elements;
      end if;
   end Has_Elements;

   --  ------------------------------
   --  Fetch the next row
   --  ------------------------------
   procedure Next (Query : in out Query_Statement) is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      Query.Proxy.Next;
   end Next;

   --  ------------------------------
   --  Returns true if the column <b>Column</b> is null.
   --  ------------------------------
   function Is_Null (Query  : in Query_Statement;
                     Column : in Natural) return Boolean is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.Is_Null (Column);
   end Is_Null;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Int64</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Int64 (Query  : Query_Statement;
                       Column : Natural) return Int64 is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Int64 is not supported by database driver";
      end if;
      return Query.Proxy.Get_Int64 (Column);
   end Get_Int64;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Integer</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Integer (Query  : Query_Statement;
                         Column : Natural) return Integer is
   begin
      if Query.Proxy = null then
         return Integer (Query_Statement'Class (Query).Get_Int64 (Column));
      else
         return Query.Proxy.Get_Integer (Column);
      end if;
   end Get_Integer;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Integer</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Natural (Query  : in Query_Statement;
                         Column : in Natural) return Natural is
   begin
      return Natural (Query.Get_Integer (Column));
   end Get_Natural;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Nullable_Integer</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Nullable_Integer (Query  : Query_Statement;
                                  Column : Natural) return Nullable_Integer is
   begin
      if Query.Proxy = null then
         return Result : Nullable_Integer do
            Result.Is_Null := Query_Statement'Class (Query).Is_Null (Column);
            if not Result.Is_Null then
               Result.Value := Integer (Query_Statement'Class (Query).Get_Int64 (Column));
            end if;
         end return;
      else
         return Query.Proxy.Get_Nullable_Integer (Column);
      end if;
   end Get_Nullable_Integer;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Float</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Float (Query  : Query_Statement;
                       Column : Natural) return Float is
   begin
      if Query.Proxy = null then
         return Float (Query_Statement'Class (Query).Get_Long_Float (Column));
      else
         return Query.Proxy.Get_Float (Column);
      end if;
   end Get_Float;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Long_Float</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Long_Float (Query  : Query_Statement;
                            Column : Natural) return Long_Float is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Double is not supported by database driver";
      else
         return Query.Proxy.Get_Long_Float (Column);
      end if;
   end Get_Long_Float;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Boolean</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Boolean (Query  : Query_Statement;
                         Column : Natural) return Boolean is
   begin
      if Query.Proxy = null then
         return Query_Statement'Class (Query).Get_Integer (Column) /= 0;
      else
         return Query.Proxy.Get_Boolean (Column);
      end if;
   end Get_Boolean;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Nullable_Boolean</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Nullable_Boolean (Query  : Query_Statement;
                                  Column : Natural) return Nullable_Boolean is
   begin
      if Query.Proxy = null then
         if Query_Statement'Class (Query).Is_Null (Column) then
            return ADO.Null_Boolean;
         else
            return (Is_Null => False,
                    Value   => Query_Statement'Class (Query).Get_Boolean (Column));
         end if;
      else
         if Query.Is_Null (Column) then
            return ADO.Null_Boolean;
         end if;
         return (Is_Null => False,
                 Value   => Query.Proxy.Get_Boolean (Column));
      end if;
   end Get_Nullable_Boolean;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Identifier</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Identifier (Query  : Query_Statement;
                            Column : Natural) return Identifier is
   begin

      if Query.Proxy = null then
         if Query_Statement'Class (Query).Is_Null (Column) then
            return ADO.NO_IDENTIFIER;
         else
            return Identifier (Query_Statement'Class (Query).Get_Int64 (Column));
         end if;
      else
         if Query.Is_Null (Column) then
            return ADO.NO_IDENTIFIER;
         end if;
         return Query.Proxy.Get_Identifier (Column);
      end if;
   end Get_Identifier;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Unbounded_String (Query  : Query_Statement;
                                  Column : Natural) return Unbounded_String is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.Get_Unbounded_String (Column);
   end Get_Unbounded_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Nullable_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Nullable_String (Query  : Query_Statement;
                                 Column : Natural) return Nullable_String is
   begin
      if Query.Proxy = null then
         return Result : Nullable_String do
            Result.Is_Null := Query_Statement'Class (Query).Is_Null (Column);
            if not Result.Is_Null then
               Result.Value := Query_Statement'Class (Query).Get_Unbounded_String (Column);
            end if;
         end return;
      else
         return Query.Proxy.Get_Nullable_String (Column);
      end if;
   end Get_Nullable_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Unbounded_String</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_String (Query  : Query_Statement;
                        Column : Natural) return String is
   begin
      if Query.Proxy = null then
         return To_String (Query_Statement'Class (Query).Get_Unbounded_String (Column));
      else
         return Query.Proxy.Get_String (Column);
      end if;
   end Get_String;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Blob</b> reference.
   --  ------------------------------
   function Get_Blob (Query  : in Query_Statement;
                      Column : in Natural) return ADO.Blob_Ref is
   begin
      if Query.Proxy = null then
         return Empty : ADO.Blob_Ref;
      else
         return Query.Proxy.Get_Blob (Column);
      end if;
   end Get_Blob;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Time (Query  : Query_Statement;
                      Column : Natural) return Ada.Calendar.Time is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.all.Get_Time (Column);
   end Get_Time;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as a <b>Nullable_Time</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Nullable_Time (Query  : in Query_Statement;
                               Column : in Natural) return Nullable_Time is
   begin
      if Query.Proxy = null then
         return Result : Nullable_Time do
            Result.Is_Null := Query_Statement'Class (Query).Is_Null (Column);
            if not Result.Is_Null then
               Result.Value := Query_Statement'Class (Query).Get_Time (Column);
            end if;
         end return;
      end if;
      return Query.Proxy.all.Get_Nullable_Time (Column);
   end Get_Nullable_Time;

   --  ------------------------------
   --  Get the column value at position <b>Column</b> and
   --  return it as an <b>Nullable_Entity_Type</b>.
   --  Raises <b>Invalid_Type</b> if the value cannot be converted.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Nullable_Entity_Type (Query  : Query_Statement;
                                      Column : Natural) return Nullable_Entity_Type is
   begin
      if Query.Proxy = null then
         return Result : Nullable_Entity_Type do
            Result.Is_Null := Query_Statement'Class (Query).Is_Null (Column);
            if not Result.Is_Null then
               Result.Value := Entity_Type (Query_Statement'Class (Query).Get_Integer (Column));
            end if;
         end return;
      end if;
      return Query.Proxy.all.Get_Nullable_Entity_Type (Column);
   end Get_Nullable_Entity_Type;

   --  ------------------------------
   --  Get the column type
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Column_Type (Query  : Query_Statement;
                             Column : Natural)
                             return ADO.Schemas.Column_Type is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.Get_Column_Type (Column);
   end Get_Column_Type;

   --  ------------------------------
   --  Get the column name.
   --  Raises <b>Invalid_Column</b> if the column does not exist.
   --  ------------------------------
   function Get_Column_Name (Query  : in Query_Statement;
                             Column : in Natural)
                             return String is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.Get_Column_Name (Column);
   end Get_Column_Name;

   --  ------------------------------
   --  Get the number of columns in the result.
   --  ------------------------------
   function Get_Column_Count (Query  : in Query_Statement)
                              return Natural is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Query statement is not initialized";
      end if;
      return Query.Proxy.Get_Column_Count;
   end Get_Column_Count;

   overriding
   procedure Adjust (Stmt : in out Query_Statement) is
   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter + 1;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Stmt : in out Query_Statement) is

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Query_Statement'Class,
                                    Name   => Query_Statement_Access);

   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter - 1;
         if Stmt.Proxy.Ref_Counter = 0 then
            Free (Stmt.Proxy);
         end if;
      end if;
   end Finalize;

   --  Execute the delete query.
   overriding
   procedure Execute (Query  : in out Delete_Statement) is
      Result : Natural;
   begin
      Log.Info ("Delete statement");

      if Query.Proxy = null then
         raise Invalid_Statement with "Delete statement not initialized";
      end if;
      Query.Proxy.Execute (Result);
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  Returns the number of rows deleted.
   --  ------------------------------
   procedure Execute (Query  : in out Delete_Statement;
                      Result : out Natural) is
   begin
      Log.Info ("Delete statement");

      if Query.Proxy = null then
         raise Invalid_Statement with "Delete statement not initialized";
      end if;
      Query.Proxy.Execute (Result);
   end Execute;

   --  ------------------------------
   --  Get the update query object associated with this update statement.
   --  ------------------------------
   function Get_Update_Query (Update : in Update_Statement)
                              return ADO.SQL.Update_Query_Access is
   begin
      return Update.Update;
   end Get_Update_Query;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Boolean) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Nullable_Boolean) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Update.Save_Field (Name => Name, Value => Value.Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Integer) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Nullable_Integer) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Update.Save_Field (Name => Name, Value => Value.Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Long_Long_Integer) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Float) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Long_Float (Value));
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Long_Float) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Identifier) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Entity_Type) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Nullable_Entity_Type) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Update.Save_Field (Name => Name, Value => Value.Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Nullable_Time) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Save_Field (Name, Value.Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in String) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Unbounded_String) is
   begin
      Update.Update.Save_Field (Name => Name, Value => Value);
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in Nullable_String) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Save_Field (Name, Value.Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in ADO.Objects.Object_Key) is
   begin
      case Value.Of_Type is
         when ADO.Objects.KEY_INTEGER =>
            declare
               V : constant Identifier := Objects.Get_Value (Value);
            begin
               Update.Update.Save_Field (Name => Name, Value => V);
            end;

         when ADO.Objects.KEY_STRING =>
            declare
               V : constant Unbounded_String := Objects.Get_Value (Value);
            begin
               Update.Update.Save_Field (Name => Name, Value => V);
            end;

      end case;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field.
   --  identified by <b>Name</b> and set it to the identifier key held by <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in ADO.Objects.Object_Ref'Class) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Save_Field (Name, Value.Get_Key);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to the <b>Value</b>.
   --  ------------------------------
   procedure Save_Field (Update : in out Update_Statement;
                         Name   : in String;
                         Value  : in ADO.Blob_Ref) is
   begin
      if Value.Is_Null then
         Update.Save_Null_Field (Name);
      else
         Update.Update.Save_Field (Name, Value);
      end if;
   end Save_Field;

   --  ------------------------------
   --  Prepare the update/insert query to save the table field
   --  identified by <b>Name</b> and set it to NULL.
   --  ------------------------------
   procedure Save_Null_Field (Update : in out Update_Statement;
                              Name   : in String) is
   begin
      Update.Update.Save_Null_Field (Name);
   end Save_Null_Field;

   --  ------------------------------
   --  Check if the update/insert query has some fields to update.
   --  ------------------------------
   function Has_Save_Fields (Update : in Update_Statement) return Boolean is
   begin
      return Update.Update.Has_Save_Fields;
   end Has_Save_Fields;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Query : in out Update_Statement) is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Update statement not initialized";
      end if;
      Query.Proxy.Execute;
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   overriding
   procedure Execute (Query : in out Insert_Statement) is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Insert statement not initialized";
      end if;
      Query.Proxy.Execute;
   end Execute;

   --  ------------------------------
   --  Execute the query
   --  ------------------------------
   procedure Execute (Query : in out Update_Statement;
                      Result : out Integer) is
   begin
      if Query.Proxy = null then
         raise Invalid_Statement with "Update statement not initialized";
      end if;
      Query.Proxy.Execute (Result);
   end Execute;

   overriding
   procedure Adjust (Stmt : in out Delete_Statement) is
   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter + 1;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Stmt : in out Delete_Statement) is

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Delete_Statement'Class,
                                    Name   => Delete_Statement_Access);
   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter - 1;
         if Stmt.Proxy.Ref_Counter = 0 then
            Free (Stmt.Proxy);
         end if;
      end if;
   end Finalize;

   overriding
   procedure Adjust (Stmt : in out Update_Statement) is
   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter + 1;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Stmt : in out Update_Statement) is

      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Update_Statement'Class,
                                    Name   => Update_Statement_Access);
   begin
      if Stmt.Proxy /= null then
         Stmt.Proxy.Ref_Counter := Stmt.Proxy.Ref_Counter - 1;
         if Stmt.Proxy.Ref_Counter = 0 then
            Free (Stmt.Proxy);
         end if;
      end if;
   end Finalize;

end ADO.Statements;
