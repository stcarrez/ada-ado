-----------------------------------------------------------------------
--  ado-cache-discrete -- Simple cache management for discrete types
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ADO.Caches.Discrete is

   --  ------------------------------
   --  Expand the name into a target parameter value to be used in the SQL query.
   --  The Expander can return a T_NULL when a value is not found or
   --  it may also raise some exception.
   --  ------------------------------
   overriding
   function Expand (Instance : in out Cache_Type;
                    Name     : in String) return ADO.Parameters.Parameter is
      Value : Element_Type;
   begin
      Value := Instance.Find (Name);
      return ADO.Parameters.Parameter '(T => ADO.Parameters.T_LONG_INTEGER,
                                        Len => 0, Value_Len => 0, Position => 0,
                                        Name => "",
                                        Long_Num => Element_Type'Pos (Value));
   end Expand;

   --  ------------------------------
   --  Find the value associated with the given name.
   --  Raises the No_Value exception if no such mapping exist.
   --  ------------------------------
   function Find (Cache : in out Cache_Type;
                  Name  : in String) return Element_Type is
   begin
      return Cache.Controller.Find (Name);
   end Find;

   --  ------------------------------
   --  Insert the value associated with the given name in the cache.
   --  When <tt>Override</tt> is set, override existing values otherwise raise an exception.
   --  ------------------------------
   procedure Insert (Cache    : in out Cache_Type;
                     Name     : in String;
                     Value    : in Element_Type;
                     Override : in Boolean := False) is
   begin
      Cache.Controller.Insert (Name, Value, Override);
   end Insert;

   --  ------------------------------
   --  Delete the value associated with the given name in the cache.
   --  Raise the No_Value exception if the value is not found and <tt>Ignore</tt> is not set.
   --  ------------------------------
   procedure Delete (Cache  : in out Cache_Type;
                     Name   : in String;
                     Ignore : in Boolean := False) is
   begin
      Cache.Controller.Delete (Name, Ignore);
   end Delete;

   --  ------------------------------
   --  Initialize the entity cache by reading the database entity table.
   --  ------------------------------
   procedure Initialize (Cache   : in out Cache_Type;
                         Session : in out ADO.Sessions.Session'Class) is
   begin
      null;
   end Initialize;

   protected body Cache_Controller is

      --  ------------------------------
      --  Find the value associated with the given name.
      --  Raises the No_Value exception if no such mapping exist.
      --  ------------------------------
      function Find (Name  : in String) return Element_Type is
         Pos : constant Cache_Map.Cursor := Values.Find (Name);
      begin
         if Cache_Map.Has_Element (Pos) then
            return Cache_Map.Element (Pos);
         else
            Log.Info ("Unknown cached value {0}", Name);
            raise No_Value with "Value '" & Name & "' not found";
         end if;
      end Find;

      --  ------------------------------
      --  Insert the value associated with the given name in the cache.
      --  When <tt>Override</tt> is set, override existing values otherwise raise an exception.
      --  ------------------------------
      procedure Insert (Name     : in String;
                        Value    : in Element_Type;
                        Override : in Boolean := False) is
         Pos : constant Cache_Map.Cursor := Values.Find (Name);
      begin
         if Cache_Map.Has_Element (Pos) then
            if not Override then
               raise No_Value;
            end if;
            Values.Replace_Element (Pos, Value);
         else
            Values.Insert (Name, Value);
         end if;
      end Insert;

      --  ------------------------------
      --  Delete the value associated with the given name in the cache.
      --  Raise the No_Value exception if the value is not found and <tt>Ignore</tt> is not set.
      --  ------------------------------
      procedure Delete (Name   : in String;
                        Ignore : in Boolean := False) is
         Pos : Cache_Map.Cursor := Values.Find (Name);
      begin
         if Cache_Map.Has_Element (Pos) then
            Values.Delete (Pos);
         elsif not Ignore then
            raise No_Value;
         end if;
      end Delete;

   end Cache_Controller;

end ADO.Caches.Discrete;
