-----------------------------------------------------------------------
--  ado.schemas -- Database Schemas
--  Copyright (C) 2009, 2010, 2018 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Ada.Strings.Equal_Case_Insensitive;
package body ADO.Schemas is

   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Column,
                                 Name   => Column_Definition);
   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Table,
                                 Name   => Table_Definition);
   procedure Free is new
     Ada.Unchecked_Deallocation (Object => Schema,
                                 Name   => Schema_Access);


   --  ------------------------------
   --  Get the hash value associated with the class mapping.
   --  ------------------------------
   function Hash (Mapping : Class_Mapping_Access) return Ada.Containers.Hash_Type is
   begin
      if Mapping = null then
         return 0;
      else
         return Util.Strings.Hash (Mapping.Table);
      end if;
   end Hash;

   --  ------------------------------
   --  Column Representation
   --  ------------------------------

   --  ------------------------------
   --  Get the column name
   --  ------------------------------
   function Get_Name (Column : Column_Definition) return String is
   begin
      return To_String (Column.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the column type
   --  ------------------------------
   function Get_Type (Column : Column_Definition) return Column_Type is
   begin
      return Column.Col_Type;
   end Get_Type;

   --  ------------------------------
   --  Get the default column value
   --  ------------------------------
   function Get_Default (Column : Column_Definition) return String is
   begin
      return To_String (Column.Default);
   end Get_Default;

   --  ------------------------------
   --  Get the column collation (for string based columns)
   --  ------------------------------
   function Get_Collation (Column : Column_Definition) return String is
   begin
      return To_String (Column.Collation);
   end Get_Collation;

   --  ------------------------------
   --  Check whether the column can be null
   --  ------------------------------
   function Is_Null (Column : Column_Definition) return Boolean is
   begin
      return Column.Is_Null;
   end Is_Null;

   --  ------------------------------
   --  Check whether the column is an unsigned number
   --  ------------------------------
   function Is_Unsigned (Column : Column_Definition) return Boolean is
   begin
      return Column.Is_Unsigned;
   end Is_Unsigned;

   --  ------------------------------
   --  Returns true if the column can hold a binary string
   --  ------------------------------
   function Is_Binary (Column : Column_Definition) return Boolean is
   begin
      return Column.Is_Binary;
   end Is_Binary;

   --  ------------------------------
   --  Returns true if the column is a primary key.
   --  ------------------------------
   function Is_Primary (Column : Column_Definition) return Boolean is
   begin
      return Column.Is_Primary;
   end Is_Primary;

   --  ------------------------------
   --  Get the column length
   --  ------------------------------
   function Get_Size (Column : Column_Definition) return Natural is
   begin
      return Column.Size;
   end Get_Size;

   --  ------------------------------
   --  Column iterator
   --  ------------------------------

   --  ------------------------------
   --  Returns true if the iterator contains more column
   --  ------------------------------
   function Has_Element (Cursor : Column_Cursor) return Boolean is
   begin
      return Cursor.Current /= null;
   end Has_Element;

   --  ------------------------------
   --  Move to the next column
   --  ------------------------------
   procedure Next (Cursor : in out Column_Cursor) is
   begin
      if Cursor.Current /= null then
         Cursor.Current := Cursor.Current.Next_Column;
      end if;
   end Next;

   --  ------------------------------
   --  Get the current column definition
   --  ------------------------------
   function Element (Cursor : Column_Cursor) return Column_Definition is
   begin
      return Cursor.Current;
   end Element;

   --  ------------------------------
   --  Table Representation
   --  ------------------------------

   --  ------------------------------
   --  Get the table name
   --  ------------------------------
   function Get_Name (Table : Table_Definition) return String is
   begin
      return To_String (Table.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the column iterator
   --  ------------------------------
   function Get_Columns (Table : Table_Definition) return Column_Cursor is
   begin
      return Column_Cursor '(Current => Table.First_Column);
   end Get_Columns;

   --  ------------------------------
   --  Find the column having the given name
   --  ------------------------------
   function Find_Column (Table : Table_Definition;
                         Name  : String) return Column_Definition is
      Column : Column_Definition := Table.First_Column;
   begin
      while Column /= null loop
         if Ada.Strings.Equal_Case_Insensitive (To_String (Column.Name), Name) then
            return Column;
         end if;
         Column := Column.Next_Column;
      end loop;
      return null;
   end Find_Column;

   --  ------------------------------
   --  Table iterator
   --  ------------------------------

   --  ------------------------------
   --  Returns true if the iterator contains more tables
   --  ------------------------------
   function Has_Element (Cursor : Table_Cursor) return Boolean is
   begin
      return Cursor.Current /= null;
   end Has_Element;

   --  ------------------------------
   --  Move to the next column
   --  ------------------------------
   procedure Next (Cursor : in out Table_Cursor) is
   begin
      if Cursor.Current /= null then
         Cursor.Current := Cursor.Current.Next_Table;
      end if;
   end Next;

   --  ------------------------------
   --  Get the current table definition
   --  ------------------------------
   function Element (Cursor : Table_Cursor) return Table_Definition is
   begin
      return Cursor.Current;
   end Element;

   --  ------------------------------
   --  Database Schema
   --  ------------------------------

   --  ------------------------------
   --  Find a table knowing its name
   --  ------------------------------
   function Find_Table (Schema : Schema_Definition;
                        Name   : String) return Table_Definition is
      Table : Table_Definition;
   begin
      if Schema.Schema /= null then
         Table := Schema.Schema.First_Table;
         while Table /= null loop
            if Ada.Strings.Equal_Case_Insensitive (To_String (Table.Name), Name) then
               return Table;
            end if;
            Table := Table.Next_Table;
         end loop;
      end if;
      return null;
   end Find_Table;

   function Get_Tables (Schema : Schema_Definition) return Table_Cursor is
   begin
      if Schema.Schema = null then
         return Table_Cursor '(Current => null);
      else
         return Table_Cursor '(Current => Schema.Schema.First_Table);
      end if;
   end Get_Tables;

   procedure Finalize (Schema : in out Schema_Definition) is
   begin
      if Schema.Schema /= null then
         declare
            Table : Table_Definition;
            Column : Column_Definition;
         begin
            loop
               Table := Schema.Schema.First_Table;
               exit when Table = null;
               loop
                  Column := Table.First_Column;
                  exit when Column = null;
                  Table.First_Column := Column.Next_Column;
                  Free (Column);
               end loop;
               Schema.Schema.First_Table := Table.Next_Table;
               Free (Table);
            end loop;
         end;
         Free (Schema.Schema);
      end if;
   end Finalize;

end ADO.Schemas;
