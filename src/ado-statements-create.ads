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

package ADO.Statements.Create is

   --  Create the query statement
   function Create_Statement (Proxy : Query_Statement_Access) return Query_Statement;

   --  Create the delete statement
   function Create_Statement (Proxy : Delete_Statement_Access) return Delete_Statement;

   --  Create an update statement
   function Create_Statement (Proxy : Update_Statement_Access) return Update_Statement;

   --  Create the insert statement.
   function Create_Statement (Proxy : Update_Statement_Access) return Insert_Statement;

end ADO.Statements.Create;
