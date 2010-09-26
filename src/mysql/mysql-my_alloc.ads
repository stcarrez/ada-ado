with Interfaces.C; use Interfaces.C;

package mysql_my_alloc_h is


  ALLOC_MAX_BLOCK_TO_DROP : constant := 4096;  --  /usr/include/mysql/my_alloc.h:23
  ALLOC_MAX_BLOCK_USAGE_BEFORE_DROP : constant := 10;  --  /usr/include/mysql/my_alloc.h:24

  -- Copyright (C) 2000 MySQL AB
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

  -- 
  --   Data structures for mysys/my_alloc.c (root memory allocator)
  -- 

  -- struct for once_alloc (block)  
  -- Next block in use  
   type st_used_mem is record
      next : access st_used_mem;  -- /usr/include/mysql/my_alloc.h:28:23
      left : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:29:16
      size : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:30:16
   end record;
   pragma Convention (C, st_used_mem);  -- /usr/include/mysql/my_alloc.h:27:1

  -- memory left in block   
  -- size of block  
   subtype USED_MEM is st_used_mem;

  -- blocks with free memory in it  
   type st_mem_root is record
      free : access st_used_mem;  -- /usr/include/mysql/my_alloc.h:36:13
      used : access st_used_mem;  -- /usr/include/mysql/my_alloc.h:37:13
      pre_alloc : access st_used_mem;  -- /usr/include/mysql/my_alloc.h:38:13
      min_malloc : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:40:16
      block_size : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:41:16
      block_num : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:42:16
      first_block_usage : aliased unsigned;  -- /usr/include/mysql/my_alloc.h:47:16
      error_handler : access procedure;  -- /usr/include/mysql/my_alloc.h:49:10
   end record;
   pragma Convention (C, st_mem_root);  -- /usr/include/mysql/my_alloc.h:35:1

  -- blocks almost without free memory  
  -- preallocated block  
  -- if block have less memory it will be put in 'used' list  
  -- initial block size  
  -- allocated blocks counter  
  -- 
  --     first free block in queue test counter (if it exceed 
  --     MAX_BLOCK_USAGE_BEFORE_DROP block will be dropped in 'used' list)
  --   

   subtype MEM_ROOT is st_mem_root;

end mysql_my_alloc_h;
