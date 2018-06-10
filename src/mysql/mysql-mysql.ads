-----------------------------------------------------------------------
--  ADO Mysql Database -- MySQL Database connections
--  Copyright (C) 2009, 2010, 2015 Stephane Carrez
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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Mysql.My_list;
with Mysql.Com;

package Mysql.Mysql is
   pragma Preelaborate;
   pragma Warnings (Off);
   pragma Style_Checks ("N");

   use Mysql;

   MAX_MYSQL_MANAGER_ERR : constant := 256;  --  /usr/include/mysql/mysql.h:330
   MAX_MYSQL_MANAGER_MSG : constant := 256;  --  /usr/include/mysql/mysql.h:331

   MANAGER_OK : constant := 200;  --  /usr/include/mysql/mysql.h:333
   MANAGER_INFO : constant := 250;  --  /usr/include/mysql/mysql.h:334
   MANAGER_ACCESS : constant := 401;  --  /usr/include/mysql/mysql.h:335
   MANAGER_CLIENT_ERR : constant := 450;  --  /usr/include/mysql/mysql.h:336
   MANAGER_INTERNAL_ERR : constant := 500;  --  /usr/include/mysql/mysql.h:337

   LOCAL_INFILE_ERROR_LEN : constant := 512;  --  /usr/include/mysql/mysql.h:463

   MYSQL_NO_DATA : constant := 100;  --  /usr/include/mysql/mysql.h:824
   MYSQL_DATA_TRUNCATED : constant := 101;  --  /usr/include/mysql/mysql.h:825

   --  Copyright (C) 2000-2003 MySQL AB
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

   --  This file defines the client API to MySQL and also the ABI of the
   --  dynamically linked libmysqlclient.
   --  The ABI should never be changed in a released product of MySQL
   --  thus you need to take great care when changing the file. In case
   --  the file is changed so the ABI is broken, you must also
   --  update the SHAREDLIB_MAJOR_VERSION in configure.in .
   --

   subtype my_bool is char;  -- /usr/include/mysql/mysql.h:51:14

   subtype gptr is Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:60:16

   -- for LISTs used in 'Mysql_Access' and 'Mysql_Access'
   mysql_port : aliased unsigned;  -- /usr/include/mysql/mysql.h:78:21
   pragma Import (Stdcall, mysql_port, "mysql_port");

   mysql_unix_port : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:79:14
   pragma Import (Stdcall, mysql_unix_port, "mysql_unix_port");

   -- Name of column
   type st_mysql_field is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:98:9
      org_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:99:9
      table : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:100:9
      org_table : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:101:9
      db : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:102:9
      catalog : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:103:9
      def : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:104:9
      length : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:105:17
      max_length : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:106:17
      name_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:107:16
      org_name_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:108:16
      table_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:109:16
      org_table_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:110:16
      db_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:111:16
      catalog_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:112:16
      def_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:113:16
      flags : aliased unsigned;  -- /usr/include/mysql/mysql.h:114:16
      decimals : aliased unsigned;  -- /usr/include/mysql/mysql.h:115:16
      charsetnr : aliased unsigned;  -- /usr/include/mysql/mysql.h:116:16
      C_Type : aliased Com.Enum_Field_Types;
   end record;
   pragma Convention (C, st_mysql_field);


   -- Original column name, if an alias
   -- Table of column if column was a field
   -- Org table name, if table was an alias
   -- Database for table
   -- Catalog for table
   -- Default value (set by mysql_list_fields)
   -- Width of column (create length)
   -- Max width for selected set
   -- Div flags
   -- Number of decimals in field
   -- Character set
   -- Type of field. See mysql_com.h for types
   type MYSQL_FIELD is access all St_Mysql_Field;

   type Row_Fields is array (Natural range <>) of Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, Row_Fields);

   type System_Access is access all System.Address;
   pragma No_Strict_Aliasing (System_Access);

   -- return data as array of strings
   type MYSQL_ROW is access all Row_Fields;
   pragma Convention (C, MYSQL_ROW);

   -- offset to current field
   subtype MYSQL_FIELD_OFFSET is unsigned;  -- /usr/include/mysql/mysql.h:121:22

--   subtype My_Ulonglong is Extensions.Unsigned_Long_Long;
   subtype My_Ulonglong is Long_Long_Integer;
   type Unsigned_Long_Long is mod 2 ** 64;

   type St_Mysql_Rows;
   type MYSQL_ROWS is access st_mysql_rows;

   -- backward compatibility define - to be removed eventually
   -- list of rows
   type st_mysql_rows is record
      next : MYSQL_ROWS;  -- /usr/include/mysql/mysql.h:139:25
      data : MYSQL_ROW;  -- /usr/include/mysql/mysql.h:140:13
      length : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:141:17
   end record;
   pragma Convention (C, st_mysql_rows);  -- /usr/include/mysql/mysql.h:138:16

   -- offset to current row
   type MYSQL_ROW_OFFSET is access all st_mysql_rows;


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

   --  skipped empty struct embedded_query_result

   type st_mysql_data is record
      rows : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:150:16
      fields : aliased unsigned;  -- /usr/include/mysql/mysql.h:151:16
      data : MYSQL_ROWS;  -- /usr/include/mysql/mysql.h:152:15
      alloc : aliased MEM_ROOT;  -- /usr/include/mysql/mysql.h:153:12
      embedded_info : System.Address;  -- /usr/include/mysql/mysql.h:155:33
   end record;
   pragma Convention (C, st_mysql_data);  -- /usr/include/mysql/mysql.h:149:16

   -- extra info for embedded library
   type MYSQL_DATA is access all st_mysql_data;

   subtype mysql_option is unsigned;
   MYSQL_OPT_CONNECT_TIMEOUT : constant mysql_option := 0;
   MYSQL_OPT_COMPRESS : constant mysql_option := 1;
   MYSQL_OPT_NAMED_PIPE : constant mysql_option := 2;
   MYSQL_INIT_COMMAND : constant mysql_option := 3;
   MYSQL_READ_DEFAULT_FILE : constant mysql_option := 4;
   MYSQL_READ_DEFAULT_GROUP : constant mysql_option := 5;
   MYSQL_SET_CHARSET_DIR : constant mysql_option := 6;
   MYSQL_SET_CHARSET_NAME : constant mysql_option := 7;
   MYSQL_OPT_LOCAL_INFILE : constant mysql_option := 8;
   MYSQL_OPT_PROTOCOL : constant mysql_option := 9;
   MYSQL_SHARED_MEMORY_BASE_NAME : constant mysql_option := 10;
   MYSQL_OPT_READ_TIMEOUT : constant mysql_option := 11;
   MYSQL_OPT_WRITE_TIMEOUT : constant mysql_option := 12;
   MYSQL_OPT_USE_RESULT : constant mysql_option := 13;
   MYSQL_OPT_USE_REMOTE_CONNECTION : constant mysql_option := 14;
   MYSQL_OPT_USE_EMBEDDED_CONNECTION : constant mysql_option := 15;
   MYSQL_OPT_GUESS_CONNECTION : constant mysql_option := 16;
   MYSQL_SET_CLIENT_IP : constant mysql_option := 17;
   MYSQL_SECURE_AUTH : constant mysql_option := 18;
   MYSQL_REPORT_DATA_TRUNCATION : constant mysql_option := 19;
   MYSQL_OPT_RECONNECT : constant mysql_option := 20;
   MYSQL_OPT_SSL_VERIFY_SERVER_CERT : constant mysql_option := 21;

   --  skipped empty struct st_dynamic_array

   type st_mysql_options is record
      connect_timeout : aliased unsigned;  -- /usr/include/mysql/mysql.h:172:16
      read_timeout : aliased unsigned;  -- /usr/include/mysql/mysql.h:172:33
      write_timeout : aliased unsigned;  -- /usr/include/mysql/mysql.h:172:47
      port : aliased unsigned;  -- /usr/include/mysql/mysql.h:173:16
      protocol : aliased unsigned;  -- /usr/include/mysql/mysql.h:173:22
      client_flag : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:174:17
      host : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:175:9
      user : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:175:15
      password : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:175:21
      unix_socket : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:175:31
      db : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:175:44
      init_commands : System.Address;  -- /usr/include/mysql/mysql.h:176:28
      my_cnf_file : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:177:9
      my_cnf_group : Interfaces.C.Strings.chars_ptr;
      charset_dir : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:177:37
      charset_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:177:51
      ssl_key : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:178:9
      ssl_cert : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:179:9
      ssl_ca : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:180:9
      ssl_capath : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:181:9
      ssl_cipher : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:182:9
      shared_memory_base_name : Interfaces.C.Strings.chars_ptr;
      max_allowed_packet : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:184:17
      use_ssl : aliased my_bool;  -- /usr/include/mysql/mysql.h:185:11
      compress : aliased my_bool;  -- /usr/include/mysql/mysql.h:186:11
      named_pipe : aliased my_bool;  -- /usr/include/mysql/mysql.h:186:20
      rpl_probe : aliased my_bool;  -- /usr/include/mysql/mysql.h:191:11
      rpl_parse : aliased my_bool;  -- /usr/include/mysql/mysql.h:196:11
      no_master_reads : aliased my_bool;  -- /usr/include/mysql/mysql.h:201:11
      separate_thread : aliased my_bool;  -- /usr/include/mysql/mysql.h:203:11
      methods_to_use : aliased mysql_option;  -- /usr/include/mysql/mysql.h:205:21
      client_ip : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:206:9
      secure_auth : aliased my_bool;  -- /usr/include/mysql/mysql.h:208:11
      report_data_truncation : aliased my_bool;  -- /usr/include/mysql/mysql.h:210:11
      local_infile_init : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address) return int;  -- /usr/include/mysql/mysql.h:213:9
      local_infile_read : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : unsigned) return int;  -- /usr/include/mysql/mysql.h:214:9
      local_infile_end : access procedure (arg1 : System.Address);
      local_infile_error : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : unsigned) return int;  -- /usr/include/mysql/mysql.h:216:9
      local_infile_userdata : System.Address;  -- /usr/include/mysql/mysql.h:217:9
   end record;
   pragma Convention (C, st_mysql_options);  -- /usr/include/mysql/mysql.h:171:8

   --  PEM key file
   --  PEM cert file
   --  PEM CA file
   --  PEM directory of CA-s?
   --  cipher to use
   --  if to use SSL or not
   --   On connect, find out the replication role of the server, and
   --   establish connections to all the peers
   --

   --   Each call to mysql_real_query() will parse it to tell if it is a read
   --   or a write, and direct it to the slave or the master
   --

   --   If set, never read from a master, only from slave, when doing
   --   a read that is replication-aware
   --

   -- Refuse client connecting to server if it uses old (pre-4.1.1) protocol
   -- 0 - never report, 1 - always report (default)
   -- function pointers for local infile support
   subtype mysql_status is unsigned;
   MYSQL_STATUS_READY : constant mysql_status := 0;
   MYSQL_STATUS_GET_RESULT : constant mysql_status := 1;
   MYSQL_STATUS_USE_RESULT : constant mysql_status := 2;

   subtype mysql_protocol_type is unsigned;
   MYSQL_PROTOCOL_DEFAULT : constant mysql_protocol_type := 0;
   MYSQL_PROTOCOL_TCP : constant mysql_protocol_type := 1;
   MYSQL_PROTOCOL_SOCKET : constant mysql_protocol_type := 2;
   MYSQL_PROTOCOL_PIPE : constant mysql_protocol_type := 3;
   MYSQL_PROTOCOL_MEMORY : constant mysql_protocol_type := 4;

   --  There are three types of queries - the ones that have to go to
   --  the master, the ones that go to a slave, and the adminstrative
   --  type which must happen on the pivot connectioin
   --

   subtype mysql_rpl_type is unsigned;
   MYSQL_RPL_MASTER : constant mysql_rpl_type := 0;
   MYSQL_RPL_SLAVE : constant mysql_rpl_type := 1;
   MYSQL_RPL_ADMIN : constant mysql_rpl_type := 2;  -- /usr/include/mysql/mysql.h:236:1

   -- character set number
   type character_set is record
      number : aliased unsigned;  -- /usr/include/mysql/mysql.h:242:21
      state : aliased unsigned;  -- /usr/include/mysql/mysql.h:243:21
      csname : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:244:22
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:245:22
      comment : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:246:22
      dir : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:247:22
      mbminlen : aliased unsigned;  -- /usr/include/mysql/mysql.h:248:21
      mbmaxlen : aliased unsigned;  -- /usr/include/mysql/mysql.h:249:21
   end record;
   pragma Convention (C, character_set);  -- /usr/include/mysql/mysql.h:241:1

   -- character set state
   -- collation name
   -- character set name
   -- comment
   -- character set directory
   -- min. length for multibyte strings
   -- max. length for multibyte strings
   subtype MY_CHARSET_INFO is character_set;

   -- Communication parameters
   -- ConnectorFd for SSL
   --  skipped empty struct charset_info_st

   type St_Mysql_Stmt;
   type MYSQL_STMT is access all St_Mysql_Stmt;

   -- needed for embedded server - no net buffer to store the 'info'
   type st_mysql;
   type Mysql_Access is access all St_Mysql;

   type anon1856_anon1888_array is array (0 .. 20) of aliased char;
   type st_mysql is record
      the_net : aliased Com.NET;  -- /usr/include/mysql/mysql.h:257:8
      connector_fd : gptr;  -- /usr/include/mysql/mysql.h:258:9
      host : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:10
      user : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:16
      passwd : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:22
      unix_socket : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:30
      server_version : Interfaces.C.Strings.chars_ptr;
      host_info : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:59
      info : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:259:70
      db : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:260:18
      charset : System.Address;  -- /usr/include/mysql/mysql.h:261:27
      fields : MYSQL_FIELD;  -- /usr/include/mysql/mysql.h:262:16
      field_alloc : aliased MEM_ROOT;
      affected_rows : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:264:16
      insert_id : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:265:16
      extra_info : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:266:16
      thread_id : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:267:17
      packet_length : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:268:17
      port : aliased unsigned;  -- /usr/include/mysql/mysql.h:269:16
      client_flag : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:270:17
      server_capabilities : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:270:29
      protocol_version : aliased unsigned;  -- /usr/include/mysql/mysql.h:271:16
      field_count : aliased unsigned;  -- /usr/include/mysql/mysql.h:272:16
      server_status : aliased unsigned;  -- /usr/include/mysql/mysql.h:273:17
      server_language : aliased unsigned;  -- /usr/include/mysql/mysql.h:274:17
      warning_count : aliased unsigned;  -- /usr/include/mysql/mysql.h:275:16
      options : aliased st_mysql_options;  -- /usr/include/mysql/mysql.h:276:27
      status : aliased mysql_status;  -- /usr/include/mysql/mysql.h:277:21
      free_me : aliased my_bool;  -- /usr/include/mysql/mysql.h:278:11
      reconnect : aliased my_bool;  -- /usr/include/mysql/mysql.h:279:11
      scramble : aliased anon1856_anon1888_array;  -- /usr/include/mysql/mysql.h:282:16
      rpl_pivot : aliased my_bool;  -- /usr/include/mysql/mysql.h:288:11
      master : Mysql_Access;  -- /usr/include/mysql/mysql.h:293:20
      next_slave : Mysql_Access;  -- /usr/include/mysql/mysql.h:293:29
      last_used_slave : Mysql_Access;  -- /usr/include/mysql/mysql.h:295:20
      last_used_con : Mysql_Access;  -- /usr/include/mysql/mysql.h:297:20
      stmts : access My_list.st_list;  -- /usr/include/mysql/mysql.h:299:10
      methods : System.Address;  -- /usr/include/mysql/mysql.h:300:34
      thd : System.Address;  -- /usr/include/mysql/mysql.h:301:9
      unbuffered_fetch_owner : access my_bool;  -- /usr/include/mysql/mysql.h:306:12
   end record;
   pragma Convention (C, st_mysql);  -- /usr/include/mysql/mysql.h:256:1

   -- id if insert on table with NEXTNR
   -- Not used
   -- Id for connection in server
   -- If free in mysql_close
   -- set to 1 if automatic reconnect
   -- session-wide random string
   --   Set if this is the original connection, not a master or a slave we have
   --   added though mysql_rpl_probe() or mysql_set_master()/ mysql_add_slave()
   --

   --    Pointers to the master, and the next slave connections, points to
   --    itself if lone connection.
   --

   -- needed for round-robin slave pick
   -- needed for send/read/store/use result to work correctly with replication
   -- list of all statements
   --    Points to boolean flag in MYSQL_RES  or Mysql_Access. We set this flag
   --    from mysql_stmt_close if close had to cancel result set of this object.
   --

   type st_mysql_res is record
      row_count : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:314:16
      fields : MYSQL_FIELD;  -- /usr/include/mysql/mysql.h:315:16
      data : MYSQL_DATA;  -- /usr/include/mysql/mysql.h:316:15
      data_cursor : MYSQL_ROWS;  -- /usr/include/mysql/mysql.h:317:15
      lengths : access unsigned_long;  -- /usr/include/mysql/mysql.h:318:18
      handle : Mysql_Access;  -- /usr/include/mysql/mysql.h:319:11
      field_alloc : aliased MEM_ROOT;
      field_count : aliased unsigned;  -- /usr/include/mysql/mysql.h:321:16
      current_field : aliased unsigned;  -- /usr/include/mysql/mysql.h:321:29
      row : MYSQL_ROW;  -- /usr/include/mysql/mysql.h:322:13
      current_row : MYSQL_ROW;  -- /usr/include/mysql/mysql.h:323:13
      eof : aliased my_bool;  -- /usr/include/mysql/mysql.h:324:11
      unbuffered_fetch_cancelled : aliased my_bool;  -- /usr/include/mysql/mysql.h:326:17
      methods : System.Address;  -- /usr/include/mysql/mysql.h:327:34
   end record;
   pragma Convention (C, st_mysql_res);  -- /usr/include/mysql/mysql.h:313:16

   -- column lengths of current row
   -- for unbuffered reads
   -- If unbuffered read
   -- buffer to current row
   -- Used by Mysql_Fetch_Row
   -- mysql_stmt_close() had to cancel this result
   type MYSQL_RES is access st_mysql_res;

   type anon1915_anon1929_array is array (0 .. 255) of aliased char;
   type st_mysql_manager is record
      the_net : aliased Com.NET;  -- /usr/include/mysql/mysql.h:346:7
      host : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:347:9
      user : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:347:15
      passwd : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:347:21
      port : aliased unsigned;  -- /usr/include/mysql/mysql.h:348:16
      free_me : aliased my_bool;  -- /usr/include/mysql/mysql.h:349:11
      eof : aliased my_bool;  -- /usr/include/mysql/mysql.h:350:11
      cmd_status : aliased int;  -- /usr/include/mysql/mysql.h:351:7
      last_errno : aliased int;  -- /usr/include/mysql/mysql.h:352:7
      net_buf : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:353:9
      net_buf_pos : Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:353:18
      net_data_end : Interfaces.C.Strings.chars_ptr;
      net_buf_size : aliased int;  -- /usr/include/mysql/mysql.h:354:7
      last_error : aliased anon1915_anon1929_array;  -- /usr/include/mysql/mysql.h:355:8
   end record;
   pragma Convention (C, st_mysql_manager);  -- /usr/include/mysql/mysql.h:345:1

   type MYSQL_MANAGER is access all st_mysql_manager;

   type st_mysql_parameters is record
      p_max_allowed_packet : access unsigned_long;  -- /usr/include/mysql/mysql.h:360:18
      p_net_buffer_length : access unsigned_long;  -- /usr/include/mysql/mysql.h:361:18
   end record;
   pragma Convention (C, st_mysql_parameters);  -- /usr/include/mysql/mysql.h:359:1

   type MYSQL_PARAMETERS is access st_mysql_parameters;

   --  Set up and bring down the server; to ensure that applications will
   --  work when linked against either the standard client library or the
   --  embedded server library, these functions should be called.
   --

   function mysql_server_init
     (arg1 : int;
      arg2 : System.Address;
      arg3 : System.Address) return int;  -- /usr/include/mysql/mysql.h:374:13
   pragma Import (Stdcall, mysql_server_init, "mysql_server_init");

   procedure mysql_server_end;  -- /usr/include/mysql/mysql.h:375:14
   pragma Import (Stdcall, mysql_server_end, "mysql_server_end");

   --  mysql_server_init/end need to be called when using libmysqld or
   --  libmysqlclient (exactly, mysql_server_init() is called by mysql_init() so
   --  you don't need to call it explicitely; but you need to call
   --  mysql_server_end() to free memory). The names are a bit misleading
   --  (mysql_SERVER* to be used when using libmysqlCLIENT). So we add more general
   --  names which suit well whether you're using libmysqld or libmysqlclient. We
   --  intend to promote these aliases over the mysql_server* ones.
   --

   function mysql_get_parameters return MYSQL_PARAMETERS;
   pragma Import (Stdcall, mysql_get_parameters, "mysql_get_parameters");

   --  Set up and bring down a thread; these function should be called
   --  for each thread in an application which opens at least one MySQL
   --  connection.  All uses of the connection(s) should be between these
   --  function calls.
   --

   function mysql_thread_init return my_bool;  -- /usr/include/mysql/mysql.h:396:17
   pragma Import (Stdcall, mysql_thread_init, "mysql_thread_init");

   procedure mysql_thread_end;  -- /usr/include/mysql/mysql.h:397:14
   pragma Import (Stdcall, mysql_thread_end, "mysql_thread_end");

   --  Functions to get information from the Mysql_Access and MYSQL_RES structures
   --  Should definitely be used if one uses shared libraries.
   --

   function Mysql_Num_Rows (arg1 : MYSQL_RES) return my_ulonglong;
   pragma Import (Stdcall, Mysql_Num_Rows, "mysql_num_rows");

   function Mysql_Num_Fields (arg1 : MYSQL_RES) return unsigned;
   pragma Import (Stdcall, Mysql_Num_Fields, "mysql_num_fields");

   function mysql_eof (arg1 : MYSQL_RES) return my_bool;
   pragma Import (Stdcall, mysql_eof, "mysql_eof");

   function Mysql_Fetch_Field_Direct (Arg1 : MYSQL_RES; Arg2 : Unsigned)
                                      return MYSQL_FIELD;
   pragma Import (Stdcall, mysql_fetch_field_direct, "mysql_fetch_field_direct");

   function Mysql_Fetch_Fields (arg1 : MYSQL_RES) return MYSQL_FIELD;
   pragma Import (Stdcall, Mysql_Fetch_Fields, "mysql_fetch_fields");

   function mysql_row_tell (arg1 : MYSQL_RES) return MYSQL_ROW_OFFSET;
   pragma Import (Stdcall, mysql_row_tell, "mysql_row_tell");

   function mysql_field_tell (arg1 : MYSQL_RES) return MYSQL_FIELD_OFFSET;
   pragma Import (Stdcall, mysql_field_tell, "mysql_field_tell");

   function mysql_field_count (arg1 : Mysql_Access) return unsigned;  -- /usr/include/mysql/mysql.h:413:22
   pragma Import (Stdcall, mysql_field_count, "mysql_field_count");

   function Mysql_Affected_Rows (arg1 : Mysql_Access) return my_ulonglong;  -- /usr/include/mysql/mysql.h:414:22
   pragma Import (Stdcall, Mysql_Affected_Rows, "mysql_affected_rows");

   function mysql_insert_id (arg1 : Mysql_Access) return my_ulonglong;  -- /usr/include/mysql/mysql.h:415:22
   pragma Import (Stdcall, mysql_insert_id, "mysql_insert_id");

   function mysql_errno (arg1 : Mysql_Access) return unsigned;  -- /usr/include/mysql/mysql.h:416:22
   pragma Import (Stdcall, mysql_errno, "mysql_errno");

   function Mysql_Error (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:417:22
   pragma Import (Stdcall, Mysql_Error, "mysql_error");

   function mysql_sqlstate (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:418:21
   pragma Import (Stdcall, mysql_sqlstate, "mysql_sqlstate");

   function mysql_warning_count (arg1 : Mysql_Access) return unsigned;  -- /usr/include/mysql/mysql.h:419:22
   pragma Import (Stdcall, mysql_warning_count, "mysql_warning_count");

   function mysql_info (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:420:22
   pragma Import (Stdcall, mysql_info, "mysql_info");

   function mysql_thread_id (arg1 : Mysql_Access) return unsigned_long;  -- /usr/include/mysql/mysql.h:421:23
   pragma Import (Stdcall, mysql_thread_id, "mysql_thread_id");

   function mysql_character_set_name (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:422:22
   pragma Import (Stdcall, mysql_character_set_name, "mysql_character_set_name");

   function mysql_set_character_set (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:423:22
   pragma Import (Stdcall, mysql_set_character_set, "mysql_set_character_set");

   function mysql_init (arg1 : Mysql_Access) return Mysql_Access;  -- /usr/include/mysql/mysql.h:425:18
   pragma Import (Stdcall, mysql_init, "mysql_init");

   function mysql_ssl_set
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr;
      arg6 : Interfaces.C.Strings.chars_ptr) return my_bool;  -- /usr/include/mysql/mysql.h:426:18
   pragma Import (Stdcall, mysql_ssl_set, "mysql_ssl_set");

   function mysql_get_ssl_cipher (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:429:25
   pragma Import (Stdcall, mysql_get_ssl_cipher, "mysql_get_ssl_cipher");

   function mysql_change_user
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr) return my_bool;  -- /usr/include/mysql/mysql.h:430:18
   pragma Import (Stdcall, mysql_change_user, "mysql_change_user");

   function mysql_real_connect
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr;
      arg6 : unsigned;
      arg7 : Interfaces.C.Strings.chars_ptr;
      arg8 : unsigned_long) return Mysql_Access;  -- /usr/include/mysql/mysql.h:432:18
   pragma Import (Stdcall, mysql_real_connect, "mysql_real_connect");

   function mysql_select_db (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:439:14
   pragma Import (Stdcall, mysql_select_db, "mysql_select_db");

   function Mysql_Query (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:440:14
   pragma Import (Stdcall, Mysql_Query, "mysql_query");

   function mysql_send_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return int;  -- /usr/include/mysql/mysql.h:441:14
   pragma Import (Stdcall, mysql_send_query, "mysql_send_query");

   function mysql_real_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return int;  -- /usr/include/mysql/mysql.h:443:14
   pragma Import (Stdcall, mysql_real_query, "mysql_real_query");

   function Mysql_Store_Result (arg1 : Mysql_Access) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:445:25
   pragma Import (Stdcall, Mysql_Store_Result, "mysql_store_result");

   function mysql_use_result (arg1 : Mysql_Access) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:446:25
   pragma Import (Stdcall, mysql_use_result, "mysql_use_result");

   -- perform query on master
   function mysql_master_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return my_bool;  -- /usr/include/mysql/mysql.h:449:18
   pragma Import (Stdcall, mysql_master_query, "mysql_master_query");

   function mysql_master_send_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return my_bool;  -- /usr/include/mysql/mysql.h:451:18
   pragma Import (Stdcall, mysql_master_send_query, "mysql_master_send_query");

   -- perform query on slave
   function mysql_slave_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return my_bool;  -- /usr/include/mysql/mysql.h:454:18
   pragma Import (Stdcall, mysql_slave_query, "mysql_slave_query");

   function mysql_slave_send_query
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return my_bool;  -- /usr/include/mysql/mysql.h:456:18
   pragma Import (Stdcall, mysql_slave_send_query, "mysql_slave_send_query");

   procedure mysql_get_character_set_info (arg1 : Mysql_Access; arg2 : access character_set);  -- /usr/include/mysql/mysql.h:458:21
   pragma Import (Stdcall, mysql_get_character_set_info, "mysql_get_character_set_info");

   -- local infile support
   procedure mysql_set_local_infile_handler
     (arg1 : Mysql_Access;
      arg2 : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address) return int;
      arg3 : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : unsigned) return int;
      arg4 : access procedure (arg1 : System.Address);
      arg5 : access function
        (arg1 : System.Address;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : unsigned) return int;
      arg6 : System.Address);  -- /usr/include/mysql/mysql.h:466:1
   pragma Import (Stdcall, mysql_set_local_infile_handler, "mysql_set_local_infile_handler");

   procedure mysql_set_local_infile_default (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:477:1
   pragma Import (Stdcall, mysql_set_local_infile_default, "mysql_set_local_infile_default");

   --  enable/disable parsing of all queries to decide if they go on master or
   --  slave
   --

   procedure mysql_enable_rpl_parse (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:484:25
   pragma Import (Stdcall, mysql_enable_rpl_parse, "mysql_enable_rpl_parse");

   procedure mysql_disable_rpl_parse (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:485:25
   pragma Import (Stdcall, mysql_disable_rpl_parse, "mysql_disable_rpl_parse");

   -- get the value of the parse flag
   function mysql_rpl_parse_enabled (arg1 : Mysql_Access) return int;  -- /usr/include/mysql/mysql.h:487:25
   pragma Import (Stdcall, mysql_rpl_parse_enabled, "mysql_rpl_parse_enabled");

   --  enable/disable reads from master
   procedure mysql_enable_reads_from_master (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:490:25
   pragma Import (Stdcall, mysql_enable_reads_from_master, "mysql_enable_reads_from_master");

   procedure mysql_disable_reads_from_master (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:491:25
   pragma Import (Stdcall, mysql_disable_reads_from_master, "mysql_disable_reads_from_master");

   -- get the value of the master read flag
   function mysql_reads_from_master_enabled (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:493:18
   pragma Import (Stdcall, mysql_reads_from_master_enabled, "mysql_reads_from_master_enabled");

   function mysql_rpl_query_type (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : int) return mysql_rpl_type;  -- /usr/include/mysql/mysql.h:495:33
   pragma Import (Stdcall, mysql_rpl_query_type, "mysql_rpl_query_type");

   -- discover the master and its slaves
   function mysql_rpl_probe (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:498:18
   pragma Import (Stdcall, mysql_rpl_probe, "mysql_rpl_probe");

   -- set the master, close/free the old one, if it is not a pivot
   function mysql_set_master
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:501:25
   pragma Import (Stdcall, mysql_set_master, "mysql_set_master");

   function mysql_add_slave
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:505:25
   pragma Import (Stdcall, mysql_add_slave, "mysql_add_slave");

   function mysql_shutdown (arg1 : Mysql_Access; arg2 : Com.mysql_enum_shutdown_level) return int;  -- /usr/include/mysql/mysql.h:510:14
   pragma Import (Stdcall, mysql_shutdown, "mysql_shutdown");

   function mysql_dump_debug_info (arg1 : Mysql_Access) return int;  -- /usr/include/mysql/mysql.h:513:14
   pragma Import (Stdcall, mysql_dump_debug_info, "mysql_dump_debug_info");

   function mysql_refresh (arg1 : Mysql_Access; arg2 : unsigned) return int;  -- /usr/include/mysql/mysql.h:514:14
   pragma Import (Stdcall, mysql_refresh, "mysql_refresh");

   function mysql_kill (arg1 : Mysql_Access; arg2 : unsigned_long) return int;  -- /usr/include/mysql/mysql.h:516:14
   pragma Import (Stdcall, mysql_kill, "mysql_kill");

   function mysql_set_server_option (arg1 : Mysql_Access; arg2 : Com.enum_mysql_set_option) return int;  -- /usr/include/mysql/mysql.h:517:14
   pragma Import (Stdcall, mysql_set_server_option, "mysql_set_server_option");

   function mysql_ping (arg1 : Mysql_Access) return int;  -- /usr/include/mysql/mysql.h:520:14
   pragma Import (Stdcall, mysql_ping, "mysql_ping");

   function mysql_stat (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:521:22
   pragma Import (Stdcall, mysql_stat, "mysql_stat");

   function mysql_get_server_info (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:522:22
   pragma Import (Stdcall, mysql_get_server_info, "mysql_get_server_info");

   function mysql_get_client_info return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:523:22
   pragma Import (Stdcall, mysql_get_client_info, "mysql_get_client_info");

   function mysql_get_client_version return unsigned_long;  -- /usr/include/mysql/mysql.h:524:23
   pragma Import (Stdcall, mysql_get_client_version, "mysql_get_client_version");

   function mysql_get_host_info (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:525:22
   pragma Import (Stdcall, mysql_get_host_info, "mysql_get_host_info");

   function mysql_get_server_version (arg1 : Mysql_Access) return unsigned_long;  -- /usr/include/mysql/mysql.h:526:23
   pragma Import (Stdcall, mysql_get_server_version, "mysql_get_server_version");

   function mysql_get_proto_info (arg1 : Mysql_Access) return unsigned;  -- /usr/include/mysql/mysql.h:527:22
   pragma Import (Stdcall, mysql_get_proto_info, "mysql_get_proto_info");

   function mysql_list_dbs (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:528:21
   pragma Import (Stdcall, mysql_list_dbs, "mysql_list_dbs");

   function mysql_list_tables (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:529:21
   pragma Import (Stdcall, mysql_list_tables, "mysql_list_tables");

   function mysql_list_processes (arg1 : Mysql_Access) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:530:21
   pragma Import (Stdcall, mysql_list_processes, "mysql_list_processes");

   function mysql_options
     (arg1 : Mysql_Access;
      arg2 : mysql_option;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:531:14
   pragma Import (Stdcall, mysql_options, "mysql_options");

   procedure Mysql_Free_Result (arg1 : MYSQL_RES);  -- /usr/include/mysql/mysql.h:533:15
   pragma Import (Stdcall, Mysql_Free_Result, "mysql_free_result");

   procedure mysql_data_seek (arg1 : MYSQL_RES; arg2 : my_ulonglong);  -- /usr/include/mysql/mysql.h:534:15
   pragma Import (Stdcall, mysql_data_seek, "mysql_data_seek");

   function mysql_row_seek (arg1 : MYSQL_RES; arg2 : MYSQL_ROW_OFFSET) return MYSQL_ROW_OFFSET;  -- /usr/include/mysql/mysql.h:536:26
   pragma Import (Stdcall, mysql_row_seek, "mysql_row_seek");

   function Mysql_Field_Seek (arg1 : MYSQL_RES; arg2 : MYSQL_FIELD_OFFSET) return MYSQL_FIELD_OFFSET;  -- /usr/include/mysql/mysql.h:538:28
   pragma Import (Stdcall, Mysql_Field_Seek, "mysql_field_seek");

   function Mysql_Fetch_Row (arg1 : MYSQL_RES) return System_Access; --  MYSQL_ROW;  -- /usr/include/mysql/mysql.h:540:19
   pragma Import (Stdcall, Mysql_Fetch_Row, "mysql_fetch_row");

   function mysql_fetch_lengths (arg1 : MYSQL_RES) return System_Access;  -- /usr/include/mysql/mysql.h:541:25
   pragma Import (Stdcall, mysql_fetch_lengths, "mysql_fetch_lengths");

   function mysql_fetch_field (arg1 : MYSQL_RES) return MYSQL_FIELD;  -- /usr/include/mysql/mysql.h:542:23
   pragma Import (Stdcall, mysql_fetch_field, "mysql_fetch_field");

   function mysql_list_fields
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:543:25
   pragma Import (Stdcall, mysql_list_fields, "mysql_list_fields");

   function mysql_escape_string
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return unsigned_long;  -- /usr/include/mysql/mysql.h:545:23
   pragma Import (Stdcall, mysql_escape_string, "mysql_escape_string");

   function mysql_hex_string
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return unsigned_long;  -- /usr/include/mysql/mysql.h:547:23
   pragma Import (Stdcall, mysql_hex_string, "mysql_hex_string");

   function mysql_real_escape_string
     (arg1 : Mysql_Access;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : unsigned_long) return unsigned_long;  -- /usr/include/mysql/mysql.h:549:23
   pragma Import (Stdcall, mysql_real_escape_string, "mysql_real_escape_string");

   procedure mysql_debug (arg1 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/mysql/mysql.h:552:15
   pragma Import (Stdcall, mysql_debug, "mysql_debug");

   procedure myodbc_remove_escape (arg1 : Mysql_Access; arg2 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/mysql/mysql.h:553:16
   pragma Import (Stdcall, myodbc_remove_escape, "myodbc_remove_escape");

   function mysql_thread_safe return unsigned;  -- /usr/include/mysql/mysql.h:554:22
   pragma Import (Stdcall, mysql_thread_safe, "mysql_thread_safe");

   function mysql_embedded return my_bool;  -- /usr/include/mysql/mysql.h:555:18
   pragma Import (Stdcall, mysql_embedded, "mysql_embedded");

   function mysql_manager_init (arg1 : MYSQL_MANAGER) return MYSQL_MANAGER;  -- /usr/include/mysql/mysql.h:556:25
   pragma Import (Stdcall, mysql_manager_init, "mysql_manager_init");

   function mysql_manager_connect
     (arg1 : MYSQL_MANAGER;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : unsigned) return MYSQL_MANAGER;  -- /usr/include/mysql/mysql.h:557:25
   pragma Import (Stdcall, mysql_manager_connect, "mysql_manager_connect");

   procedure mysql_manager_close (arg1 : MYSQL_MANAGER);  -- /usr/include/mysql/mysql.h:562:25
   pragma Import (Stdcall, mysql_manager_close, "mysql_manager_close");

   function mysql_manager_command
     (arg1 : MYSQL_MANAGER;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return int;  -- /usr/include/mysql/mysql.h:563:25
   pragma Import (Stdcall, mysql_manager_command, "mysql_manager_command");

   function mysql_manager_fetch_line
     (arg1 : MYSQL_MANAGER;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : int) return int;  -- /usr/include/mysql/mysql.h:565:25
   pragma Import (Stdcall, mysql_manager_fetch_line, "mysql_manager_fetch_line");

   function mysql_read_query_result (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:568:25
   pragma Import (Stdcall, mysql_read_query_result, "mysql_read_query_result");

   --  The following definitions are added for the enhanced
   --  client-server protocol
   --

   -- statement state
   subtype enum_mysql_stmt_state is unsigned;
   MYSQL_STMT_INIT_DONE : constant enum_mysql_stmt_state := 1;
   MYSQL_STMT_PREPARE_DONE : constant enum_mysql_stmt_state := 2;
   MYSQL_STMT_EXECUTE_DONE : constant enum_mysql_stmt_state := 3;
   MYSQL_STMT_FETCH_DONE : constant enum_mysql_stmt_state := 4;  -- /usr/include/mysql/mysql.h:578:1

   --  This structure is used to define bind information, and
   --  internally by the client library.
   --  Public members with their descriptions are listed below
   --  (conventionally `On input' refers to the binds given to
   --  mysql_stmt_bind_param, `On output' refers to the binds given
   --  to mysql_stmt_bind_result):
   --  buffer_type    - One of the MYSQL_* types, used to describe
   --                   the host language type of buffer.
   --                   On output: if column type is different from
   --                   buffer_type, column value is automatically converted
   --                   to buffer_type before it is stored in the buffer.
   --  buffer         - On input: points to the buffer with input data.
   --                   On output: points to the buffer capable to store
   --                   output data.
   --                   The type of memory pointed by buffer must correspond
   --                   to buffer_type. See the correspondence table in
   --                   the comment to mysql_stmt_bind_param.
   --  The two above members are mandatory for any kind of bind.
   --  buffer_length  - the length of the buffer. You don't have to set
   --                   it for any fixed length buffer: float, double,
   --                   int, etc. It must be set however for variable-length
   --                   types, such as BLOBs or STRINGs.
   --  length         - On input: in case when lengths of input values
   --                   are different for each execute, you can set this to
   --                   point at a variable containining value length. This
   --                   way the value length can be different in each execute.
   --                   If length is not NULL, buffer_length is not used.
   --                   Note, length can even point at buffer_length if
   --                   you keep bind structures around while fetching:
   --                   this way you can change buffer_length before
   --                   each execution, everything will work ok.
   --                   On output: if length is set, mysql_stmt_fetch will
   --                   write column length into it.
   --  is_null        - On input: points to a boolean variable that should
   --                   be set to TRUE for NULL values.
   --                   This member is useful only if your data may be
   --                   NULL in some but not all cases.
   --                   If your data is never NULL, is_null should be set to 0.
   --                   If your data is always NULL, set buffer_type
   --                   to MYSQL_TYPE_NULL, and is_null will not be used.
   --  is_unsigned    - On input: used to signify that values provided for one
   --                   of numeric types are unsigned.
   --                   On output describes signedness of the output buffer.
   --                   If, taking into account is_unsigned flag, column data
   --                   is out of range of the output buffer, data for this column
   --                   is regarded truncated. Note that this has no correspondence
   --                   to the sign of result set column, if you need to find it out
   --                   use mysql_stmt_result_metadata.
   --  error          - where to write a truncation error if it is present.
   --                   possible error value is:
   --                   0  no truncation
   --                   1  value is out of range or buffer is too small
   --  Please note that MYSQL_BIND also has internals members.
   --
   type st_mysql_bind;
   type MYSQL_BIND is access St_Mysql_Bind;

   -- output length pointer
   type st_mysql_bind is record
      length : access unsigned_long;  -- /usr/include/mysql/mysql.h:649:18
      is_null : access my_bool;  -- /usr/include/mysql/mysql.h:650:18
      buffer : System.Address;  -- /usr/include/mysql/mysql.h:651:10
      error : access my_bool;  -- /usr/include/mysql/mysql.h:653:18
      buffer_type : aliased Com.enum_field_types;  -- /usr/include/mysql/mysql.h:654:25
      buffer_length : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:656:17
      row_ptr : access unsigned_char;  -- /usr/include/mysql/mysql.h:657:18
      offset : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:658:17
      length_value : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:659:17
      param_number : aliased unsigned;  -- /usr/include/mysql/mysql.h:660:16
      pack_length : aliased unsigned;  -- /usr/include/mysql/mysql.h:661:17
      error_value : aliased my_bool;  -- /usr/include/mysql/mysql.h:662:17
      is_unsigned : aliased my_bool;  -- /usr/include/mysql/mysql.h:663:17
      long_data_used : aliased my_bool;  -- /usr/include/mysql/mysql.h:664:11
      is_null_value : aliased my_bool;  -- /usr/include/mysql/mysql.h:665:11
      store_param_func : access procedure (arg1 : access Com.st_net; arg2 : MYSQL_BIND);  -- /usr/include/mysql/mysql.h:666:10
      fetch_result : access procedure
        (arg1 : MYSQL_BIND;
         arg2 : MYSQL_FIELD;
         arg3 : System.Address);  -- /usr/include/mysql/mysql.h:667:10
      skip_result : access procedure
        (arg1 : MYSQL_BIND;
         arg2 : MYSQL_FIELD;
         arg3 : System.Address);  -- /usr/include/mysql/mysql.h:669:10
   end record;
   pragma Convention (C, st_mysql_bind);  -- /usr/include/mysql/mysql.h:648:1

   -- Pointer to null indicator
   -- buffer to get/put data
   -- set this if you want to track data truncations happened during fetch
   -- buffer type
   -- output buffer length, must be set when fetching str/binary
   -- for the current data position
   -- offset position for char/binary fetch
   -- Used if length is 0
   -- For null count and error messages
   -- Internal length for packed data
   -- used if error is 0
   -- set if integer type is unsigned
   -- If used with mysql_send_long_data
   -- Used if is_null is 0

   -- statement handler
   -- root allocations
   type anon1855_anon2254_array is array (0 .. 511) of aliased char;
   type anon1855_anon2255_array is array (0 .. 5) of aliased char;
   type st_mysql_stmt is record
      the_mem_root : aliased MEM_ROOT;  -- /usr/include/mysql/mysql.h:677:18
      the_list : aliased My_List.LIST;  -- /usr/include/mysql/mysql.h:678:18
      the_mysql : Mysql_Access;  -- /usr/include/mysql/mysql.h:679:19
      params : MYSQL_BIND;  -- /usr/include/mysql/mysql.h:680:19
      bind : MYSQL_BIND;  -- /usr/include/mysql/mysql.h:681:19
      fields : MYSQL_FIELD;  -- /usr/include/mysql/mysql.h:682:19
      result : aliased MYSQL_DATA;  -- /usr/include/mysql/mysql.h:683:18
      data_cursor : MYSQL_ROWS;  -- /usr/include/mysql/mysql.h:684:19
      affected_rows : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:686:18
      insert_id : aliased my_ulonglong;  -- /usr/include/mysql/mysql.h:687:18
      read_row_func : access function (arg1 : MYSQL_STMT; arg2 : System.Address) return int;  -- /usr/include/mysql/mysql.h:692:20
      stmt_id : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:694:18
      flags : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:695:18
      prefetch_rows : aliased unsigned_long;  -- /usr/include/mysql/mysql.h:696:18
      server_status : aliased unsigned;  -- /usr/include/mysql/mysql.h:701:18
      last_errno : aliased unsigned;  -- /usr/include/mysql/mysql.h:702:17
      param_count : aliased unsigned;  -- /usr/include/mysql/mysql.h:703:18
      field_count : aliased unsigned;  -- /usr/include/mysql/mysql.h:704:18
      state : aliased enum_mysql_stmt_state;  -- /usr/include/mysql/mysql.h:705:30
      last_error : aliased anon1855_anon2254_array;  -- /usr/include/mysql/mysql.h:706:10
      sqlstate : aliased anon1855_anon2255_array;  -- /usr/include/mysql/mysql.h:707:10
      send_types_to_server : aliased my_bool;  -- /usr/include/mysql/mysql.h:709:18
      bind_param_done : aliased my_bool;  -- /usr/include/mysql/mysql.h:710:18
      bind_result_done : aliased unsigned_char;  -- /usr/include/mysql/mysql.h:711:18
      unbuffered_fetch_cancelled : aliased my_bool;  -- /usr/include/mysql/mysql.h:713:17
      update_max_length : aliased my_bool;  -- /usr/include/mysql/mysql.h:718:17
   end record;
   pragma Convention (C, st_mysql_stmt);  -- /usr/include/mysql/mysql.h:253:8

   -- list to keep track of all stmts
   -- connection handle
   -- input parameters
   -- output parameters
   -- result set metadata
   -- cached result set
   -- current row in cached result
   -- copy of mysql->affected_rows after statement execution
   -- copy of mysql->insert_id
   --    mysql_stmt_fetch() calls this function to fetch one row (it's different
   --    for buffered, unbuffered and cursor fetch).
   --

   -- Id for prepared statement
   -- i.e. type of cursor to open
   -- number of rows per one COM_FETCH
   --    Copied from mysql->server_status after execute/fetch to know
   --    server-side cursor status for this statement.
   --

   -- error code
   -- input parameter count
   -- number of columns in result set
   -- statement state
   -- error message
   -- Types of input parameters should be sent to server
   -- input buffers were supplied
   -- output buffers were supplied
   -- mysql_stmt_close() had to cancel this result
   --    Is set to true if we need to calculate field->max_length for
   --    metadata fields when doing mysql_stmt_store_result.
   --

   subtype enum_stmt_attr_type is unsigned;
   STMT_ATTR_UPDATE_MAX_LENGTH : constant enum_stmt_attr_type := 0;
   STMT_ATTR_CURSOR_TYPE : constant enum_stmt_attr_type := 1;
   STMT_ATTR_PREFETCH_ROWS : constant enum_stmt_attr_type := 2;  -- /usr/include/mysql/mysql.h:722:1

   --    When doing mysql_stmt_store_result calculate max_length attribute
   --    of statement metadata. This is to be consistent with the old API,
   --    where this was done automatically.
   --    In the new API we do that only by request because it slows down
   --    mysql_stmt_store_result sufficiently.
   --

   --    unsigned long with combination of cursor flags (read only, for update,
   --    etc)
   --

   --    Amount of rows to retrieve from server per one fetch if using cursors.
   --    Accepts unsigned long attribute in the range 1 - ulong_max
   --

   type st_mysql_methods is record
      read_query_result : access function (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:746:13
      advanced_command : access function
        (arg1 : Mysql_Access;
         arg2 : Com.enum_server_command;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : unsigned_long;
         arg5 : Interfaces.C.Strings.chars_ptr;
         arg6 : unsigned_long;
         arg7 : my_bool;
         arg8 : MYSQL_STMT) return my_bool;  -- /usr/include/mysql/mysql.h:747:13
      read_rows : access function
        (arg1 : Mysql_Access;
         arg2 : MYSQL_FIELD;
         arg3 : unsigned) return MYSQL_DATA;  -- /usr/include/mysql/mysql.h:755:17
      use_result : access function (arg1 : Mysql_Access) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:757:17
      fetch_lengths : access procedure
        (arg1 : access unsigned_long;
         arg2 : MYSQL_ROW;
         arg3 : unsigned);  -- /usr/include/mysql/mysql.h:758:10
      flush_use_result : access procedure (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:760:10
      list_fields : access function (arg1 : Mysql_Access) return MYSQL_FIELD;  -- /usr/include/mysql/mysql.h:762:19
      read_prepare_result : access function (arg1 : Mysql_Access; arg2 : MYSQL_STMT) return my_bool;  -- /usr/include/mysql/mysql.h:763:13
      stmt_execute : access function (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:764:9
      read_binary_rows : access function (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:765:9
      unbuffered_fetch : access function (arg1 : Mysql_Access; arg2 : System.Address) return int;  -- /usr/include/mysql/mysql.h:766:9
      free_embedded_thd : access procedure (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:767:10
      read_statistics : access function (arg1 : Mysql_Access) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:768:17
      next_result : access function (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:769:13
      read_change_user_result : access function
        (arg1 : Mysql_Access;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/mysql/mysql.h:770:9
      read_rows_from_cursor : access function (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:771:9
   end record;
   pragma Convention (C, st_mysql_methods);  -- /usr/include/mysql/mysql.h:252:8

   subtype MYSQL_METHODS is st_mysql_methods;

   function mysql_stmt_init (arg1 : Mysql_Access) return MYSQL_STMT;  -- /usr/include/mysql/mysql.h:776:22
   pragma Import (Stdcall, mysql_stmt_init, "mysql_stmt_init");

   function mysql_stmt_prepare
     (arg1 : MYSQL_STMT;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return int;  -- /usr/include/mysql/mysql.h:777:13
   pragma Import (Stdcall, mysql_stmt_prepare, "mysql_stmt_prepare");

   function mysql_stmt_execute (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:779:13
   pragma Import (Stdcall, mysql_stmt_execute, "mysql_stmt_execute");

   function mysql_stmt_fetch (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:780:13
   pragma Import (Stdcall, mysql_stmt_fetch, "mysql_stmt_fetch");

   function mysql_stmt_fetch_column
     (arg1 : MYSQL_STMT;
      arg2 : MYSQL_BIND;
      arg3 : unsigned;
      arg4 : unsigned_long) return int;  -- /usr/include/mysql/mysql.h:781:13
   pragma Import (Stdcall, mysql_stmt_fetch_column, "mysql_stmt_fetch_column");

   function mysql_stmt_store_result (arg1 : MYSQL_STMT) return int;  -- /usr/include/mysql/mysql.h:784:13
   pragma Import (Stdcall, mysql_stmt_store_result, "mysql_stmt_store_result");

   function mysql_stmt_param_count (arg1 : MYSQL_STMT) return unsigned_long;  -- /usr/include/mysql/mysql.h:785:23
   pragma Import (Stdcall, mysql_stmt_param_count, "mysql_stmt_param_count");

   function mysql_stmt_attr_set
     (arg1 : MYSQL_STMT;
      arg2 : enum_stmt_attr_type;
      arg3 : System.Address) return my_bool;  -- /usr/include/mysql/mysql.h:786:17
   pragma Import (Stdcall, mysql_stmt_attr_set, "mysql_stmt_attr_set");

   function mysql_stmt_attr_get
     (arg1 : MYSQL_STMT;
      arg2 : enum_stmt_attr_type;
      arg3 : System.Address) return my_bool;  -- /usr/include/mysql/mysql.h:789:17
   pragma Import (Stdcall, mysql_stmt_attr_get, "mysql_stmt_attr_get");

   function mysql_stmt_bind_param (arg1 : MYSQL_STMT; arg2 : MYSQL_BIND) return my_bool;  -- /usr/include/mysql/mysql.h:792:17
   pragma Import (Stdcall, mysql_stmt_bind_param, "mysql_stmt_bind_param");

   function mysql_stmt_bind_result (arg1 : MYSQL_STMT; arg2 : MYSQL_BIND) return my_bool;  -- /usr/include/mysql/mysql.h:793:17
   pragma Import (Stdcall, mysql_stmt_bind_result, "mysql_stmt_bind_result");

   function mysql_stmt_close (arg1 : MYSQL_STMT) return my_bool;  -- /usr/include/mysql/mysql.h:794:17
   pragma Import (Stdcall, mysql_stmt_close, "mysql_stmt_close");

   function mysql_stmt_reset (arg1 : MYSQL_STMT) return my_bool;  -- /usr/include/mysql/mysql.h:795:17
   pragma Import (Stdcall, mysql_stmt_reset, "mysql_stmt_reset");

   function mysql_stmt_free_result (arg1 : MYSQL_STMT) return my_bool;  -- /usr/include/mysql/mysql.h:796:17
   pragma Import (Stdcall, mysql_stmt_free_result, "mysql_stmt_free_result");

   function mysql_stmt_send_long_data
     (arg1 : MYSQL_STMT;
      arg2 : unsigned;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : unsigned_long) return my_bool;  -- /usr/include/mysql/mysql.h:797:17
   pragma Import (Stdcall, mysql_stmt_send_long_data, "mysql_stmt_send_long_data");

   function mysql_stmt_result_metadata (arg1 : MYSQL_STMT) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:801:20
   pragma Import (Stdcall, mysql_stmt_result_metadata, "mysql_stmt_result_metadata");

   function mysql_stmt_param_metadata (arg1 : MYSQL_STMT) return MYSQL_RES;  -- /usr/include/mysql/mysql.h:802:20
   pragma Import (Stdcall, mysql_stmt_param_metadata, "mysql_stmt_param_metadata");

   function mysql_stmt_errno (arg1 : MYSQL_STMT) return unsigned;  -- /usr/include/mysql/mysql.h:803:22
   pragma Import (Stdcall, mysql_stmt_errno, "mysql_stmt_errno");

   function mysql_stmt_error (arg1 : MYSQL_STMT) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:804:21
   pragma Import (Stdcall, mysql_stmt_error, "mysql_stmt_error");

   function mysql_stmt_sqlstate (arg1 : MYSQL_STMT) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/mysql/mysql.h:805:21
   pragma Import (Stdcall, mysql_stmt_sqlstate, "mysql_stmt_sqlstate");

   function mysql_stmt_row_seek (arg1 : MYSQL_STMT; arg2 : MYSQL_ROW_OFFSET) return MYSQL_ROW_OFFSET;  -- /usr/include/mysql/mysql.h:806:26
   pragma Import (Stdcall, mysql_stmt_row_seek, "mysql_stmt_row_seek");

   function mysql_stmt_row_tell (arg1 : MYSQL_STMT) return MYSQL_ROW_OFFSET;  -- /usr/include/mysql/mysql.h:808:26
   pragma Import (Stdcall, mysql_stmt_row_tell, "mysql_stmt_row_tell");

   procedure mysql_stmt_data_seek (arg1 : MYSQL_STMT; arg2 : my_ulonglong);  -- /usr/include/mysql/mysql.h:809:14
   pragma Import (Stdcall, mysql_stmt_data_seek, "mysql_stmt_data_seek");

   function mysql_stmt_num_rows (arg1 : MYSQL_STMT) return my_ulonglong;  -- /usr/include/mysql/mysql.h:810:22
   pragma Import (Stdcall, mysql_stmt_num_rows, "mysql_stmt_num_rows");

   function mysql_stmt_affected_rows (arg1 : MYSQL_STMT) return my_ulonglong;  -- /usr/include/mysql/mysql.h:811:22
   pragma Import (Stdcall, mysql_stmt_affected_rows, "mysql_stmt_affected_rows");

   function mysql_stmt_insert_id (arg1 : MYSQL_STMT) return my_ulonglong;  -- /usr/include/mysql/mysql.h:812:22
   pragma Import (Stdcall, mysql_stmt_insert_id, "mysql_stmt_insert_id");

   function mysql_stmt_field_count (arg1 : MYSQL_STMT) return unsigned;  -- /usr/include/mysql/mysql.h:813:22
   pragma Import (Stdcall, mysql_stmt_field_count, "mysql_stmt_field_count");

   function mysql_commit (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:815:17
   pragma Import (Stdcall, mysql_commit, "mysql_commit");

   function mysql_rollback (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:816:17
   pragma Import (Stdcall, mysql_rollback, "mysql_rollback");

   function mysql_autocommit (arg1 : Mysql_Access; arg2 : my_bool) return my_bool;  -- /usr/include/mysql/mysql.h:817:17
   pragma Import (Stdcall, mysql_autocommit, "mysql_autocommit");

   function mysql_more_results (arg1 : Mysql_Access) return my_bool;  -- /usr/include/mysql/mysql.h:818:17
   pragma Import (Stdcall, mysql_more_results, "mysql_more_results");

   function mysql_next_result (arg1 : Mysql_Access) return int;  -- /usr/include/mysql/mysql.h:819:13
   pragma Import (Stdcall, mysql_next_result, "mysql_next_result");

   procedure mysql_close (arg1 : Mysql_Access);  -- /usr/include/mysql/mysql.h:820:14
   pragma Import (Stdcall, mysql_close, "mysql_close");

   --  status return codes
   --  The following functions are mainly exported because of mysqlbinlog;
   --  They are not for general usage
   --

end Mysql.Mysql;
