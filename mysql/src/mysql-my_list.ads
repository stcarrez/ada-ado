with Interfaces.C; use Interfaces.C;
with System;

package Mysql.My_list is

   pragma Preelaborate;

   --  arg-macro: function list_rest (a)
   --    return (a).next;
   --  arg-macro: function list_push (a, b)
   --    return a):=list_cons((b),(a);
   --  Copyright (C) 2000 MySQL AB
   --   This program is free software; you can redistribute it and/or modify
   --   it under the terms of the GNU General Public License as published by
   --   the Free Software Foundation; version 2 of the License.
   --   This program is distributed in the hope that it will be useful,
   --   but WITHOUT ANY WARRANTY; without even the implied warranty of
   --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   --   GNU General Public License for more details.
   --   You should have received a copy of the GNU General Public License
   --   along with this program; if not, write to the Free Software
   --   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   --  /usr/include/mysql/my_list.h:24:19
   type st_list is record
      prev : access st_list;
      next : access st_list;
      data : System.Address;
   end record;
   pragma Convention (C, st_list);

   subtype LIST is st_list;

   type List_Walk_Action is
     access function (arg1 : System.Address; arg2 : System.Address) return int;

   function list_add (arg1 : access st_list; arg2 : access st_list) return access st_list;
   pragma Import (C, list_add, "list_add");

   function list_delete (arg1 : access st_list; arg2 : access st_list) return access st_list;
   pragma Import (C, list_delete, "list_delete");

   function list_cons (arg1 : System.Address; arg2 : access st_list) return access st_list;
   pragma Import (C, list_cons, "list_cons");

   function list_reverse (arg1 : access st_list) return access st_list;
   pragma Import (C, list_reverse, "list_reverse");

   procedure list_free (arg1 : access st_list; arg2 : unsigned);
   pragma Import (C, list_free, "list_free");

   function list_length (arg1 : access st_list) return unsigned;
   pragma Import (C, list_length, "list_length");

end Mysql.My_list;
