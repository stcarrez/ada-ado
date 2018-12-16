-----------------------------------------------------------------------
--  ado-schemas -- Database Schemas
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
with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Containers;
with Util.Strings;
with ADO.Configs;
package ADO.Schemas is

   type Column_Index is new Natural range 0 .. ADO.Configs.MAX_COLUMNS;

   type Member_Names is array (Column_Index range <>) of Util.Strings.Name_Access;

   type Class_Mapping (Count : Column_Index)
   is tagged limited record
      Table     : Util.Strings.Name_Access;
      Members   : Member_Names (1 .. Count);
   end record;

   type Class_Mapping_Access is access constant Class_Mapping'Class;

   --  Get the hash value associated with the class mapping.
   function Hash (Mapping : Class_Mapping_Access) return Ada.Containers.Hash_Type;

   --  Get the Ada type mapping for the column
   type Column_Type is
     (
      T_UNKNOWN,

      --  Boolean column
      T_BOOLEAN,

      T_TINYINT,
      T_SMALLINT,
      T_INTEGER,
      T_LONG_INTEGER,

      T_FLOAT,
      T_DOUBLE,

      T_DECIMAL,

      T_ENUM,
      T_SET,

      T_TIME,
      T_YEAR,
      T_DATE,
      T_DATE_TIME,
      T_TIMESTAMP,

      T_CHAR,
      T_VARCHAR,
      T_BLOB,

      T_NULL
     );

   --  ------------------------------
   --  Column Representation
   --  ------------------------------
   --  Describes a column in a table.
   type Column_Definition is private;

   --  Get the column name
   function Get_Name (Column : Column_Definition) return String;

   --  Get the column type
   function Get_Type (Column : Column_Definition) return Column_Type;

   --  Get the default column value
   function Get_Default (Column : Column_Definition) return String;

   --  Get the column collation (for string based columns)
   function Get_Collation (Column : Column_Definition) return String;

   --  Check whether the column can be null
   function Is_Null (Column : Column_Definition) return Boolean;

   --  Check whether the column is an unsigned number
   function Is_Unsigned (Column : Column_Definition) return Boolean;

   --  Returns true if the column can hold a binary string
   function Is_Binary (Column : Column_Definition) return Boolean;

   --  Returns true if the column is a primary key.
   function Is_Primary (Column : Column_Definition) return Boolean;

   --  Get the column length
   function Get_Size (Column : Column_Definition) return Natural;

   --  ------------------------------
   --  Column iterator
   --  ------------------------------
   type Column_Cursor is private;

   --  Returns true if the iterator contains more column
   function Has_Element (Cursor : Column_Cursor) return Boolean;

   --  Move to the next column
   procedure Next (Cursor : in out Column_Cursor);

   --  Get the current column definition
   function Element (Cursor : Column_Cursor) return Column_Definition;

   --  ------------------------------
   --  Table Representation
   --  ------------------------------
   --  Describes a table in the database.  The table contains a list
   --  of columns described by Column_Definition.
   type Table_Definition is private;

   --  Get the table name
   function Get_Name (Table : Table_Definition) return String;

   --  Get the column iterator
   function Get_Columns (Table : Table_Definition) return Column_Cursor;

   --  Find the column having the given name
   function Find_Column (Table : Table_Definition;
                         Name  : String) return Column_Definition;

   --  ------------------------------
   --  Table iterator
   --  ------------------------------
   type Table_Cursor is private;

   --  Returns true if the iterator contains more tables
   function Has_Element (Cursor : Table_Cursor) return Boolean;

   --  Move to the next column
   procedure Next (Cursor : in out Table_Cursor);

   --  Get the current table definition
   function Element (Cursor : Table_Cursor) return Table_Definition;

   --  ------------------------------
   --  Database Schema
   --  ------------------------------

   type Schema_Definition is limited private;

   --  Find a table knowing its name
   function Find_Table (Schema : Schema_Definition;
                        Name   : String) return Table_Definition;

   function Get_Tables (Schema : Schema_Definition) return Table_Cursor;

private
   use Ada.Strings.Unbounded;

   type Column;
   type Column_Definition is access all Column;

   type Table;
   type Table_Definition is access all Table;

   type Schema;
   type Schema_Access is access all Schema;

   type Schema_Definition is new Ada.Finalization.Limited_Controlled with record
      Schema : Schema_Access;
   end record;

   procedure Finalize (Schema : in out Schema_Definition);

   type Column_Cursor is record
      Current    : Column_Definition;
   end record;

   type Table_Cursor is record
      Current    : Table_Definition;
   end record;

   type Column is record
      Next_Column : Column_Definition;
      Table       : Table_Definition;
      Name        : Unbounded_String;
      Default     : Unbounded_String;
      Collation   : Unbounded_String;
      Col_Type    : Column_Type   := T_VARCHAR;
      Size        : Natural       := 0;
      Is_Null     : Boolean       := False;
      Is_Binary   : Boolean       := False;
      Is_Unsigned : Boolean       := False;
      Is_Primary  : Boolean       := False;
   end record;

   type Table is record
      Name         : Unbounded_String;
      First_Column : Column_Definition;
      Next_Table   : Table_Definition;
   end record;

   type Schema is record
      --        Tables : Table_Definition;
      First_Table : Table_Definition;
   end record;

end ADO.Schemas;
