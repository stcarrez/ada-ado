-----------------------------------------------------------------------
--  ADO Statements -- Database statements
--  Copyright (C) 2009, 2010, 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ADO.Statements.Create is

   --  ------------------------------
   --  Create the query statement
   --  ------------------------------
   function Create_Statement (Proxy    : in Query_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Query_Statement is
   begin
      Proxy.Set_Expander (Expander);
      Proxy.Query.Set_Expander (Expander);
      return Result : Query_Statement do
         Result.Query := Proxy.Get_Query;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   --  ------------------------------
   --  Create the delete statement
   --  ------------------------------
   function Create_Statement (Proxy    : in Delete_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Delete_Statement is
   begin
      Proxy.Set_Expander (Expander);
      Proxy.Query.Set_Expander (Expander);
      return Result : Delete_Statement do
         Result.Query := Proxy.Get_Query;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   --  ------------------------------
   --  Create an update statement
   --  ------------------------------
   function Create_Statement (Proxy    : in Update_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Update_Statement is
   begin
      Proxy.Set_Expander (Expander);
      Proxy.Update.Set_Expander (Expander);
      return Result : Update_Statement do
         Result.Update := Proxy.Get_Update_Query;
         Result.Query := Result.Update.all'Access;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   --  ------------------------------
   --  Create the insert statement.
   --  ------------------------------
   function Create_Statement (Proxy    : in Update_Statement_Access;
                              Expander : in ADO.Parameters.Expander_Access := null)
                              return Insert_Statement is
   begin
      Proxy.Set_Expander (Expander);
      Proxy.Update.Set_Expander (Expander);
      return Result : Insert_Statement do
         Result.Update := Proxy.Get_Update_Query;
         Result.Query := Result.Update.all'Access;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

end ADO.Statements.Create;
