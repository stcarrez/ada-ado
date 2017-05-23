-----------------------------------------------------------------------
--  ADO Parameters -- Parameters for queries
--  Copyright (C) 2010, 2011, 2012, 2013, 2017 Stephane Carrez
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
with Util.Log.Loggers;

with Ada.Calendar.Formatting;
package body ADO.Parameters is

   use Parameter_Vectors;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ADO.Parameters");

   --  --------------------
   --  Set the SQL dialect description object.
   --  --------------------
   procedure Set_Dialect (Params : in out Abstract_List;
                          D      : in ADO.Drivers.Dialects.Dialect_Access) is
   begin
      Params.Dialect := D;
   end Set_Dialect;

   --  --------------------
   --  Set the cache expander to be used when expanding the SQL.
   --  --------------------
   procedure Set_Expander (Params   : in out Abstract_List;
                           Expander : in ADO.Parameters.Expander_Access) is
   begin
      Params.Expander := Expander;
   end Set_Expander;

   --  --------------------
   --  Get the SQL dialect description object.
   --  --------------------
   function Get_Dialect (From : in Abstract_List) return ADO.Drivers.Dialects.Dialect_Access is
   begin
      return From.Dialect;
   end Get_Dialect;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Boolean) is
      Param : constant Parameter := Parameter '(T         => T_BOOLEAN,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Bool      => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Integer) is
      Param : constant Parameter := Parameter '(T         => T_INTEGER,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Num       => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Long_Long_Integer) is
      Param : constant Parameter := Parameter '(T         => T_LONG_INTEGER,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Long_Num  => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in Identifier) is
      Param : constant Parameter := Parameter '(T         => T_LONG_INTEGER,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Long_Num  => Long_Long_Integer (Value));
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in ADO.Entity_Type) is
      Param : constant Parameter := Parameter '(T         => T_LONG_INTEGER,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Long_Num  => Long_Long_Integer (Value));
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in String) is
   begin
      Abstract_List'Class (Params).
        Add_Parameter (Parameter '(T         => T_STRING,
                                   Len       => Name'Length,
                                   Value_Len => Value'Length,
                                   Name      => Name,
                                   Position  => 0,
                                   Str       => Value));
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in Token) is
   begin
      Abstract_List'Class (Params).
        Add_Parameter (Parameter '(T         => T_TOKEN,
                                   Len       => Name'Length,
                                   Value_Len => Value'Length,
                                   Name      => Name,
                                   Position  => 0,
                                   Str       => String (Value)));
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in ADO.Blob_Ref) is
   begin
      Abstract_List'Class (Params).
        Add_Parameter (Parameter '(T         => T_BLOB,
                                   Len       => Name'Length,
                                   Value_Len => 0,
                                   Name      => Name,
                                   Position  => 0,
                                   Data      => Value));
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in ADO.Utils.Identifier_Vector) is
      S : constant String := ADO.Utils.To_Parameter_List (Value);
   begin
      Abstract_List'Class (Params).
        Add_Parameter (Parameter '(T         => T_LIST,
                                   Len       => Name'Length,
                                   Value_Len => S'Length,
                                   Name      => Name,
                                   Position  => 0,
                                   Str       => S));
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in Unbounded_String) is
      Val   : constant String := To_String (Value);
      Param : constant Parameter := Parameter '(T         => T_STRING,
                                                Len       => Name'Length,
                                                Value_Len => Val'Length,
                                                Name      => Name,
                                                Position  => 0,
                                                Str       => Val);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time) is
      Param : constant Parameter := Parameter '(T         => T_DATE,
                                                Len       => Name'Length,
                                                Value_Len => 0,
                                                Name      => Name,
                                                Position  => 0,
                                                Time      => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Boolean) is
      P : Parameter := Parameter '(T         => T_BOOLEAN,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Bool      => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Long_Long_Integer) is
      P : Parameter := Parameter '(T         => T_LONG_INTEGER,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Long_Num  => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Identifier) is
      P : Parameter := Parameter '(T         => T_LONG_INTEGER,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Long_Num  => Long_Long_Integer (Value));
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Integer) is
      P : Parameter := Parameter '(T         => T_INTEGER,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Num       => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Entity_Type) is
      P : Parameter := Parameter '(T         => T_INTEGER,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Num       => Integer (Value));
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in String) is
      P : Parameter := Parameter '(T         => T_STRING,
                                   Len       => 0,
                                   Value_Len => Value'Length,
                                   Name      => "",
                                   Position  => Position,
                                   Str       => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Unbounded_String) is
      Val : constant String := To_String (Value);
      P : Parameter := Parameter '(T         => T_STRING,
                                   Len       => 0,
                                   Value_Len => Val'Length,
                                   Name      => "",
                                   Position  => Position,
                                   Str       => Val);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params    : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Ada.Calendar.Time) is
      P : Parameter := Parameter '(T         => T_DATE,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Time      => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in ADO.Blob_Ref) is
      P : Parameter := Parameter '(T         => T_BLOB,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position,
                                   Data      => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Null_Param (Params   : in out Abstract_List;
                              Position : in Natural) is
      P : Parameter := Parameter '(T         => T_NULL,
                                   Len       => 0,
                                   Value_Len => 0,
                                   Name      => "",
                                   Position  => Position);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Null_Param;

   procedure Bind_Null_Param (Params   : in out Abstract_List;
                              Name     : in String) is
      P : constant Parameter := Parameter '(T         => T_NULL,
                                            Len       => Name'Length,
                                            Value_Len => 0,
                                            Name      => Name,
                                            Position  => 0);
   begin
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Null_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value  : in Boolean) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Long_Long_Integer) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Identifier) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Integer) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in String) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Unbounded_String) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Ada.Calendar.Time) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   --  ------------------------------
   --  Add a null parameter.
   --  ------------------------------
   procedure Add_Null_Param (Params : in out Abstract_List) is
   begin
      Params.Bind_Null_Param (Position => 0);
   end Add_Null_Param;

   --  ------------------------------
   --  Expand the SQL string with the query parameters.  The following parameters syntax
   --  are recognized and replaced:
   --  <ul>
   --     <li>? is replaced according to the current parameter index.  The index is incremented
   --         after each occurrence of ? character.
   --     <li>:nnn is replaced by the parameter at index <b>nnn</b>.
   --     <li>:name is replaced by the parameter with the name <b>name</b>
   --     <li>$group[var-name] is replaced by using the expander interface.
   --  </ul>
   --  Parameter strings are escaped.  When a parameter is not found, an empty string is used.
   --  Returns the expanded SQL string.
   --  ------------------------------
   function Expand (Params : in Abstract_List'Class;
                    SQL    : in String) return String is
      use ADO.Parameters;

      --  Format and append the date to the buffer.
      procedure Append_Date (Buffer : in out Unbounded_String;
                             Time   : in Ada.Calendar.Time);

      --  Replace the given parameter.
      procedure Replace_Parameter (Param : in Parameter);

      --  Find and replace the parameter identified by the name.
      procedure Replace_Parameter (Name : in String);

      --  Find and replace the parameter at the given index.
      procedure Replace_Parameter (Position : in Natural);

      --  Find and replace the variable by using the expander and looking up in the
      --  group identified by <tt>Group</tt> a value with the name <tt>Name</tt>.
      procedure Replace_Expander (Group : in String;
                                  Name  : in STring);

      --  ------------------------------
      --  Format and append the date to the buffer.
      --  ------------------------------
      procedure Append_Date (Buffer : in out Unbounded_String;
                             Time   : in Ada.Calendar.Time) is
      begin
         Append (Buffer, "'");
         Append (Buffer, Ada.Calendar.Formatting.Image (Time, True));
         Append (Buffer, "'");
      end Append_Date;

      Max    : constant Natural := Params.Length;
      Buffer : Unbounded_String;

      --  ------------------------------
      --  Replace the given parameter.
      --  ------------------------------
      procedure Replace_Parameter (Param : in Parameter) is
      begin
         case Param.T is
            when T_LONG_INTEGER =>
               Append (Buffer, Util.Strings.Image (Param.Long_Num));

            when T_INTEGER =>
               Append (Buffer, Util.Strings.Image (Param.Num));

            when T_BOOLEAN =>
               if Param.Bool then
                  Append (Buffer, '1');
               else
                  Append (Buffer, '0');
               end if;

            when T_DATE =>
               Append_Date (Buffer, Param.Time);

            when T_NULL =>
               Append (Buffer, "NULL");

            when T_TOKEN | T_LIST =>
               Append (Buffer, Param.Str);

            when T_BLOB =>
               Append (Buffer, ''');
               Params.Dialect.Escape_Sql (Buffer, Param.Data);
               Append (Buffer, ''');

            when others =>
               Append (Buffer, ''');
               Params.Dialect.Escape_Sql (Buffer, Param.Str);
               Append (Buffer, ''');
         end case;
      end Replace_Parameter;

      --  ------------------------------
      --  Find and replace the parameter at the given index.
      --  ------------------------------
      procedure Replace_Parameter (Position : in Natural) is
      begin
         if Position = 0 or Position > Max then
            Log.Warn ("Invalid parameter '{0}' in query '{1}'",
                      Natural'Image (Position), SQL);
         else
            Params.Query_Element (Position => Position,
                                  Process  => Replace_Parameter'Access);
         end if;
      end Replace_Parameter;

      --  ------------------------------
      --  Find and replace the parameter identified by the name.
      --  ------------------------------
      procedure Replace_Parameter (Name : in String) is

         --  Check if the parameter matches and replace it.
         procedure Process (Param : in Parameter);

         Found : Boolean := False;

         --  ------------------------------
         --  Check if the parameter matches and replace it.
         --  ------------------------------
         procedure Process (Param : in Parameter) is
         begin
            if Param.Name = Name then
               Replace_Parameter (Param);
               Found := True;
            end if;
         end Process;

      begin
         --  We assume that in most queries, the number of parameters is relatively small
         --  (1, 2 or 3) and even complex queries should not have a lot of parameters.
         --  To avoid the cost and complexity of hash map, we use a simple search.
         for I in 1 .. Max loop
            Params.Query_Element (I, Process'Access);
            if Found then
               return;
            end if;
         end loop;

         Log.Warn ("Parameter '{0}' not found in query '{1}'", Name, SQL);
      end Replace_Parameter;

      --  ------------------------------
      --  Find and replace the variable by using the expander and looking up in the
      --  group identified by <tt>Group</tt> a value with the name <tt>Name</tt>.
      --  ------------------------------
      procedure Replace_Expander (Group : in String;
                                  Name  : in STring) is
      begin
         if Params.Expander = null then
            Log.Warn ("There is no expander to evaluate ${0}[{1}]", Group, Name);
         else
            Replace_Parameter (Params.Expander.Expand (Group, Name));
         end if;

      exception
         when others =>
            Log.Warn ("No value ${0}[{1}] in the cache expander", Group, Name);

      end Replace_Expander;

      Num       : Natural := 0;
      Pos       : Natural;
      C         : Character;
      First_Pos : Natural;
   begin
      --  Build the SQL query by injecting the parameters.
      Pos := SQL'First;
      First_Pos := Pos;
      while Pos <= SQL'Last loop
         C := SQL (Pos);
         if C = '\' then
            if First_Pos <= Pos - 1 then
               Append (Buffer, SQL (First_Pos .. Pos - 1));
            end if;
            First_Pos := Pos + 1;
            exit when Pos + 1 > SQL'Last;
            Pos := Pos + 2;

         elsif C = ':' and Pos + 1 <= SQL'Last then
            if First_Pos <= Pos - 1 then
               Append (Buffer, SQL (First_Pos .. Pos - 1));
            end if;
            Pos := Pos + 1;
            C := SQL (Pos);
            if C >= '0' and C <= '9' then
               Num := 0;
               loop
                  Num := Num * 10 + Natural (Character'Pos (C) - Character'Pos ('0'));
                  Pos := Pos + 1;
                  exit when Pos > SQL'Last;
                  C := SQL (Pos);
                  exit when not (C >= '0' and C <= '9');
               end loop;
               Replace_Parameter (Position => Num);
               First_Pos := Pos;

            else
               declare
                  Start_Pos : constant Natural := Pos;
               begin
                  --  Isolate the parameter name.
                  loop
                     exit when not (C >= 'a' and C <= 'z') and not (C >= 'A' and C <= 'Z')
                       and not (C >= '0' and C <= '9') and C /= '_';
                     Pos := Pos + 1;
                     exit when Pos > SQL'Last;
                     C := SQL (Pos);
                  end loop;

                  --  And replace it with its value.
                  Replace_Parameter (Name => SQL (Start_Pos .. Pos - 1));
                  First_Pos := Pos;
               end;
            end if;

         elsif C = '?' then
            if First_Pos <= Pos - 1 then
               Append (Buffer, SQL (First_Pos .. Pos - 1));
            end if;
            Num := Num + 1;
            Replace_Parameter (Position => Num);
            Pos := Pos + 1;
            First_Pos := Pos;

         elsif C = '$' then
            if First_Pos <= Pos - 1 then
               Append (Buffer, SQL (First_Pos .. Pos - 1));
            end if;

            --  We have a variable to lookup in a cache with the form: $group[var-name]
            Pos := Pos + 1;
            declare
               Group_Start : constant Natural := Pos;
               Group_End   : Natural;
            begin
               --  Isolate the parameter name.
               loop
                  C := SQL (Pos);
                  exit when C = '[';
                  Pos := Pos + 1;
                  exit when Pos > SQL'Last;
                  C := SQL (Pos);
               end loop;
               if C = '[' then
                  Group_End := Pos;
                  loop
                     C := SQL (Pos);
                     exit when C = ']';
                     Pos := Pos + 1;
                     exit when Pos > SQL'Last;
                     C := SQL (Pos);
                  end loop;
                  if C = ']' then
                     Replace_Expander (Group => SQL (Group_Start .. Group_End - 1),
                                       Name  => SQL (Group_End + 1 .. Pos - 1));
                     Pos := Pos + 1;
                  end if;
               end if;

               --  And replace it with its value.
               First_Pos := Pos;
            end;

         else
            Pos := Pos + 1;
         end if;
      end loop;
      if First_Pos <= SQL'Last then
         Append (Buffer, SQL (First_Pos .. SQL'Last));
      end if;
      return To_String (Buffer);
   end Expand;

   function Compare_On_Name (Left, Right : in Parameter) return Boolean is
   begin
      return Left.Name = Right.Name;
   end Compare_On_Name;

   --  ------------------------------
   --  Add the parameter in the list.
   --  ------------------------------
   procedure Add_Parameter (Params : in out List;
                            Param  : in Parameter) is
      Pos : Parameter_Vectors.Extended_Index;
   begin
      if Param.Position = 0 then
         Pos := Params.Params.Find_Index (Param);
         if Pos /= Parameter_Vectors.No_Index then
            Params.Params.Replace_Element (Index    => Pos,
                                           New_Item => Param);
         else
            Params.Params.Append (Param);
         end if;
      elsif Param.Position = Natural (Length (Params.Params)) + 1 then
         Params.Params.Append (Param);
      else
         Params.Params.Replace_Element (Index    => Param.Position,
                                        New_Item => Param);
      end if;
   end Add_Parameter;

   --  ------------------------------
   --  Return the number of parameters in the list.
   --  ------------------------------
   function Length (Params : in List) return Natural is
   begin
      return Natural (Length (Params.Params));
   end Length;

   --  ------------------------------
   --  Clear the list of parameters.
   --  ------------------------------
   procedure Clear (Params : in out List) is
   begin
      Parameter_Vectors.Clear (Params.Params);
   end Clear;

   --  ------------------------------
   --  Set the parameters from another parameter list.
   --  ------------------------------
   procedure Set_Parameters (Params : in out List;
                             From   : in Abstract_List'Class) is
      L : constant List'Class := List'Class (From);
   begin
      Params.Params := L.Params;
   end Set_Parameters;

   --  ------------------------------
   --  Return the parameter at the given position
   --  ------------------------------
   function Element (Params   : in List;
                     Position : in Natural) return Parameter is
   begin
      return Element (Params.Params, Position);
   end Element;

   --  ------------------------------
   --  Execute the <b>Process</b> procedure with the given parameter as argument.
   --  ------------------------------
   procedure Query_Element (Params   : in List;
                            Position : in Natural;
                            Process  : not null access procedure (Element : in Parameter)) is
   begin
      Query_Element (Params.Params, Position, Process);
   end Query_Element;

end ADO.Parameters;
