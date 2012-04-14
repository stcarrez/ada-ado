-----------------------------------------------------------------------
--  ADO Parameters -- Parameters for queries
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

with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;

--  Defines a list of parameters for an SQL statement.
--
package ADO.Parameters is

   use Ada.Strings.Unbounded;

   type Token is new String;

   type Parameter_Type is (T_NULL, T_STRING, T_TOKEN, T_DATE, T_LONG_INTEGER,
                           T_INTEGER, T_BOOLEAN, T_BLOB);

   type Parameter (T   : Parameter_Type;
                   Len : Natural;
                   Value_Len : Natural) is record
      Position : Natural := 0;
      Name     : String (1 .. Len);
      case T is
         when T_NULL =>
            null;

         when T_LONG_INTEGER =>
            Long_Num : Long_Long_Integer := 0;

         when T_INTEGER =>
            Num : Integer;

         when T_BOOLEAN =>
            Bool : Boolean;

         when T_DATE =>
            Time : Ada.Calendar.Time;

         when T_BLOB =>
            Data : ADO.Blob_Ref;

         when others =>
            Str : String (1 .. Value_Len);
      end case;
   end record;

   type Abstract_List is abstract new Ada.Finalization.Controlled with private;
   type Abstract_List_Access is access all Abstract_List'Class;

   --  Add the parameter in the list.
   procedure Add_Parameter (Params : in out Abstract_List;
                            Param  : in Parameter) is abstract;

   --  Set the parameters from another parameter list.
   procedure Set_Parameters (Parameters : in out Abstract_List;
                             From       : in Abstract_List'Class) is abstract;

   --  Return the number of parameters in the list.
   function Length (Params : in Abstract_List) return Natural is abstract;

   --  Return the parameter at the given position
   function Element (Params   : in Abstract_List;
                     Position : in Natural) return Parameter is abstract;

   --  Execute the <b>Process</b> procedure with the given parameter as argument.
   procedure Query_Element (Params   : in Abstract_List;
                            Position : in Natural;
                            Process  : not null access
                              procedure (Element : in Parameter)) is abstract;

   --  Clear the list of parameters.
   procedure Clear (Parameters : in out Abstract_List) is abstract;

   --  Operations to bind a parameter
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Boolean);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Long_Long_Integer);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Identifier);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Integer);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in ADO.Entity_Type);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in String);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Unbounded_String);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Ada.Calendar.Time);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in Token);
   procedure Bind_Param (Params : in out Abstract_List;
                         Name   : in String;
                         Value  : in ADO.Blob_Ref);

   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Boolean);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Long_Long_Integer);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Identifier);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Integer);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Entity_Type);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in String);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Unbounded_String);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in Ada.Calendar.Time);
   procedure Bind_Param (Params   : in out Abstract_List;
                         Position : in Natural;
                         Value    : in ADO.Blob_Ref);
   procedure Bind_Null_Param (Params   : in out Abstract_List;
                              Position : in Natural);
   procedure Bind_Null_Param (Params   : in out Abstract_List;
                              Name     : in String);

   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Boolean);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Long_Long_Integer);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Identifier);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Integer);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in String);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Unbounded_String);
   procedure Add_Param (Params : in out Abstract_List;
                         Value : in Ada.Calendar.Time);

   --  Expand the SQL string with the query parameters.  The following parameters syntax
   --  are recognized and replaced:
   --  <ul>
   --     <li>? is replaced according to the current parameter index.  The index is incremented
   --         after each occurrence of ? character.
   --     <li>:nnn is replaced by the parameter at index <b>nnn</b>.
   --     <li>:name is replaced by the parameter with the name <b>name</b>
   --  </ul>
   --  Parameter strings are escaped.  When a parameter is not found, an empty string is used.
   --  Returns the expanded SQL string.
   function Expand (Params : in Abstract_List'Class;
                    SQL    : in String) return String;

   --  ------------------------------
   --  List of parameters
   --  ------------------------------
   --  The <b>List</b> is an implementation of the parameter list.
   type List is new Abstract_List with private;

   procedure Add_Parameter (Params : in out List;
                            Param  : in Parameter);

   procedure Set_Parameters (Params : in out List;
                             From   : in Abstract_List'Class);

   --  Return the number of parameters in the list.
   function Length (Params : in List) return Natural;

   --  Return the parameter at the given position
   function Element (Params   : in List;
                     Position : in Natural) return Parameter;

   --  Execute the <b>Process</b> procedure with the given parameter as argument.
   procedure Query_Element (Params   : in List;
                            Position : in Natural;
                            Process  : not null access procedure (Element : in Parameter));

   --  Clear the list of parameters.
   procedure Clear (Params : in out List);

private

   package Parameter_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Parameter);

   type Abstract_List is abstract new Ada.Finalization.Controlled with null record;

   type List is new Abstract_List with record
      Params : Parameter_Vectors.Vector;
   end record;

end ADO.Parameters;
