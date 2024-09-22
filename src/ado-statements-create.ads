-----------------------------------------------------------------------
--  ado-statements-create -- Create database statements
--  Copyright (C) 2009, 2010, 2011, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package ADO.Statements.Create is

   --  Create the query statement
   function Create_Statement (Proxy    : in Query_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Query_Statement;

   --  Create the delete statement
   function Create_Statement (Proxy    : in Delete_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Delete_Statement;

   --  Create an update statement
   function Create_Statement (Proxy    : in Update_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Update_Statement;

   --  Create the insert statement.
   function Create_Statement (Proxy    : in Update_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Insert_Statement;

end ADO.Statements.Create;
