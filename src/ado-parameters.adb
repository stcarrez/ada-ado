-----------------------------------------------------------------------
--  ADO Parameters -- Parameters for queries
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

--  with GNAT.Calendar.Time_IO;      use GNAT.Calendar.Time_IO;
with Util.Log;
with Util.Log.Loggers;
with Ada.Calendar.Formatting;
package body ADO.Parameters is

   use Util.Log;

   use Parameter_Vectors;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Statements");

   --
   function "+"(Str : in String)
                     return Unbounded_String renames To_Unbounded_String;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Boolean) is
      Param : constant Parameter := Parameter '(T        => T_Boolean,
                                                Name     => + (Name),
                                                Position => 0,
                                                Bool     => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Integer) is
      Param : constant Parameter := Parameter '(T        => T_INTEGER,
                                                Name     => +(Name),
                                                Position => 0,
                                                Num      => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Long_Integer) is
      Param : constant Parameter := Parameter '(T        => T_LONG_INTEGER,
                                                Name     => +(Name),
                                                Position => 0,
                                                Long_Num => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in Identifier) is
      Param : constant Parameter := Parameter '(T        => T_LONG_INTEGER,
                                                Name     => +(Name),
                                                Position => 0,
                                                Long_Num => Long_Integer (Value));
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in String) is
   begin
      Abstract_List'Class (Params).
	Add_Parameter (Parameter '(T    => T_STRING,
				   Name => To_Unbounded_String (Name),
				   Position => 0,
				   Str      => To_Unbounded_String (Value)));
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name  : in String;
                         Value : in Unbounded_String) is
      Param : constant Parameter := Parameter '(T => T_STRING, Name => +(Name),
                                                Position => 0, Str => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time) is
      Param : constant Parameter := Parameter '(T => T_DATE, Name => +(Name),
                                                Position => 0, Time => Value);
   begin
      Abstract_List'Class (Params).Add_Parameter (Param);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Boolean) is
      P : Parameter := Parameter '(T        => T_BOOLEAN,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Bool     => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Long_Integer) is
      P : Parameter := Parameter '(T        => T_LONG_INTEGER,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Long_Num => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Identifier) is
      P : Parameter := Parameter '(T        => T_LONG_INTEGER,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Long_Num => Long_Integer (Value));
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Integer) is
      P : Parameter := Parameter '(T        => T_INTEGER,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Num      => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in String) is
      P : Parameter := Parameter '(T        => T_STRING,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Str      => +(Value));
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Unbounded_String) is
      P : Parameter := Parameter '(T        => T_STRING,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Str      => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Bind_Param (Params    : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Ada.Calendar.Time) is
      P : Parameter := Parameter '(T        => T_DATE,
                                   Name     => Null_Unbounded_String,
                                   Position => Position,
                                   Time     => Value);
   begin
      if Position = 0 then
         P.Position := Abstract_List'Class (Params).Length + 1;
      end if;
      Abstract_List'Class (Params).Add_Parameter (P);
   end Bind_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value  : in Boolean) is
   begin
      Params.Bind_Param (Position => 0, Value => Value);
   end Add_Param;

   procedure Add_Param (Params : in out Abstract_List;
                        Value : in Long_Integer) is
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

   --  Expand the query parameters
   function Expand (Params : in Abstract_List;
                    SQL    : in String) return String is

      procedure Escape_Sql (Buffer : in out Unbounded_String;
                            Item   : in String) is
         C  : Character;
      begin
         for I in Item'Range loop
            C := Item (I);
            if C = '\' or C = ASCII.CR or C = ''' or C = '"' then
               Append (Buffer, '\');
            end if;
            Append (Buffer, C);
         end loop;
      end Escape_Sql;

      procedure Append_Date (Buffer : in out Unbounded_String;
                             Time   : in Ada.Calendar.Time) is
      begin
         Append (Buffer, "'");
         Append (Buffer,
                 Ada.Calendar.Formatting.Image (Time, True));
         Append (Buffer, "'");
      end Append_Date;

      Num    : Natural := 0;
      Pos    : Natural;
      C      : Character;
      Buffer : Unbounded_String;

      use ADO.Parameters;

   begin
      --  Build the SQL query by injecting the parameters.
      Pos := Sql'First;
      while Pos <= Sql'Last loop
         C := Sql (Pos);
         if C = '\' then
            Append (Buffer, C);
            Pos := Pos + 1;
            exit when Pos > Sql'Last;
            Append (Buffer, Sql (Pos));

         elsif C = ':' or C = '?' then
            if C = ':' then
               Pos := Pos + 1;
               exit when Pos > Sql'Last;
               C := Sql (Pos);
               Num := Natural (Character'Pos (C) - Character'Pos ('0'));
            else
               Num := Num + 1;
            end if;
            declare
               P   : constant Parameter := Abstract_List'Class (Params).Element (Num);
            begin
               case P.T is
                  when T_LONG_INTEGER =>
                     Append (Buffer, Long_Integer'Image (P.Long_Num));

                  when T_INTEGER =>
                     Append (Buffer, Integer'Image (P.Num));

                  when T_BOOLEAN =>
                     if P.Bool then
                        Append (Buffer, "1");
                     else
                        Append (Buffer, "0");
                     end if;

                  when T_DATE =>
                     Append_Date (Buffer, P.Time);

                  when others =>
                     Append (Buffer, "'");
                     Escape_Sql (Buffer, To_String (P.Str));
                     Append (Buffer, "'");
               end case;
            end;
         else
            Append (Buffer, C);
         end if;
         Pos := Pos + 1;
      end loop;
      return To_String (Buffer);
   end Expand;

   --  ------------------------------
   --  Add the parameter in the list.
   --  ------------------------------
   procedure Add_Parameter (Params : in out List;
                            Param  : in Parameter) is
   begin
      if Param.Position = Natural (Length (Params.Params)) + 1 then
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

end ADO.Parameters;
