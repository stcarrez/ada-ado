-----------------------------------------------------------------------
--  ADO Statements -- Database statements
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Util.Log;
with Util.Log.Loggers;
with System.Storage_Elements;
with Ada.Unchecked_Deallocation;
package body ADO.Statements.Create is

   use Util.Log;
   use System.Storage_Elements;

   Log : constant Loggers.Logger := Loggers.Create ("ADO.Statements");

   function Create_Statement (Proxy : Query_Statement_Access) return Query_Statement is
   begin
      return Result : Query_Statement do
         Result.Query := Proxy.Get_Query;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   function Create_Statement (Proxy : Delete_Statement_Access) return Delete_Statement is
   begin
      return Result : Delete_Statement do
         Result.Query := Proxy.Get_Query;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   function Create_Statement (Proxy : Update_Statement_Access) return Update_Statement is
   begin
      return Result : Update_Statement do
         Result.Update := Proxy.Get_Update_Query;
         Result.Query := Result.Update.all'Access;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

   function Create_Statement (Proxy : Update_Statement_Access) return Insert_Statement is
   begin
      return Result : Insert_Statement do
         Result.Update := Proxy.Get_Update_Query;
         Result.Query := Result.Update.all'Access;
         Result.Proxy := Proxy;
         Result.Proxy.Ref_Counter := 1;
      end return;
   end Create_Statement;

end ADO.Statements.Create;
