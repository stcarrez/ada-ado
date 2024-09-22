-----------------------------------------------------------------------
--  ado-utils-serialize -- Utility operations for JSON/XML serialization
--  Copyright (C) 2016, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ADO.Schemas;
package body ADO.Utils.Serialize is

   --  ------------------------------
   --  Write the entity to the serialization stream (JSON/XML).
   --  ------------------------------
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Identifier) is
   begin
      Stream.Write_Long_Entity (Name, Long_Long_Integer (Value));
   end Write_Entity;

   --  ------------------------------
   --  Write the entity to the serialization stream (JSON/XML).
   --  ------------------------------
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Objects.Object_Key) is
   begin
      case Value.Of_Type is
         when ADO.Objects.KEY_INTEGER =>
            Write_Entity (Stream, Name, ADO.Objects.Get_Value (Value));

         when ADO.Objects.KEY_STRING =>
            Stream.Write_Entity (Name, ADO.Objects.Get_Value (Value));

      end case;
   end Write_Entity;

   --  ------------------------------
   --  Write the SQL query results to the serialization stream (JSON/XML).
   --  ------------------------------
   procedure Write_Query (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                          Name   : in String;
                          Query  : in out ADO.Statements.Query_Statement) is
      use ADO.Schemas;
      Column_Count : Natural;
   begin
      Stream.Start_Array (Name);
      if Query.Has_Elements then
         Column_Count := Query.Get_Column_Count;
         while Query.Has_Elements loop
            Stream.Start_Entity ("");
            for I in 1 .. Column_Count loop
               declare
                  Name : constant String := Query.Get_Column_Name (I - 1);
               begin
                  if Query.Is_Null (I - 1) then
                     Stream.Write_Null_Entity (Name);
                  else
                     case Query.Get_Column_Type (I - 1) is
                        --  Boolean column
                     when T_BOOLEAN =>
                        Stream.Write_Entity (Name, Query.Get_Boolean (I - 1));

                     when T_TINYINT | T_SMALLINT | T_INTEGER | T_LONG_INTEGER | T_YEAR =>
                        Stream.Write_Entity (Name, Query.Get_Integer (I - 1));

                     when T_FLOAT | T_DOUBLE | T_DECIMAL =>
                        Stream.Write_Null_Entity (Name);

                     when T_ENUM =>
                        Stream.Write_Entity (Name, Query.Get_String (I - 1));

                     when T_TIME | T_DATE | T_DATE_TIME | T_TIMESTAMP =>
                        Stream.Write_Entity (Name, Query.Get_Time (I - 1));

                     when T_CHAR | T_VARCHAR =>
                        Stream.Write_Entity (Name, Query.Get_String (I - 1));

                     when T_BLOB =>
                        Stream.Write_Null_Entity (Name);

                     when T_SET | T_UNKNOWN | T_NULL =>
                        Stream.Write_Null_Entity (Name);

                     end case;

                  end if;
               end;
            end loop;
            Stream.End_Entity ("");
            Query.Next;
         end loop;
      end if;
      Stream.End_Array (Name);
   end Write_Query;

end ADO.Utils.Serialize;
