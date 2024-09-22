-----------------------------------------------------------------------
--  ado-utils-serialize -- Utility operations for JSON/XML serialization
--  Copyright (C) 2016, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Serialize.IO;
with ADO.Objects;
with ADO.Statements;

package ADO.Utils.Serialize is

   --  Write the entity to the serialization stream (JSON/XML).
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Identifier);

   --  Write the entity to the serialization stream (JSON/XML).
   procedure Write_Entity (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                           Name   : in String;
                           Value  : in ADO.Objects.Object_Key);

   --  Write the SQL query results to the serialization stream (JSON/XML).
   procedure Write_Query (Stream : in out Util.Serialize.IO.Output_Stream'Class;
                          Name   : in String;
                          Query  : in out ADO.Statements.Query_Statement);

end ADO.Utils.Serialize;
