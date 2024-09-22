-----------------------------------------------------------------------
--  ADO Mysql -- Mysql Interface
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;

package Mysql.Com is
   pragma Preelaborate;

   pragma Warnings (Off);
   pragma Style_Checks ("N");

   subtype my_socket is int;  -- /usr/include/mysql/mysql.h:66:13

   NAME_LEN : constant := 64;  --  /usr/include/mysql/mysql_com.h:23
   HOSTNAME_LENGTH : constant := 60;  --  /usr/include/mysql/mysql_com.h:24
   USERNAME_LENGTH : constant := 16;  --  /usr/include/mysql/mysql_com.h:25
   SERVER_VERSION_LENGTH : constant := 60;  --  /usr/include/mysql/mysql_com.h:26
   SQLSTATE_LENGTH : constant := 5;  --  /usr/include/mysql/mysql_com.h:27
   --  unsupported macro: USER_HOST_BUFF_SIZE HOSTNAME_LENGTH + USERNAME_LENGTH + 2

   LOCAL_HOST : aliased constant String := "localhost" & ASCII.NUL;  --  /usr/include/mysql/mysql_com.h:37
   LOCAL_HOST_NAMEDPIPE : aliased constant String := "." & ASCII.NUL;  --  /usr/include/mysql/mysql_com.h:38

   SCRAMBLE_LENGTH : constant := 20;  --  /usr/include/mysql/mysql_com.h:71
   SCRAMBLE_LENGTH_323 : constant := 8;  --  /usr/include/mysql/mysql_com.h:72
   --  unsupported macro: SCRAMBLED_PASSWORD_CHAR_LENGTH (SCRAMBLE_LENGTH*2+1)
   --  unsupported macro: SCRAMBLED_PASSWORD_CHAR_LENGTH_323 (SCRAMBLE_LENGTH_323*2)

   NOT_NULL_FLAG : constant := 1;  --  /usr/include/mysql/mysql_com.h:78
   PRI_KEY_FLAG : constant := 2;  --  /usr/include/mysql/mysql_com.h:79
   UNIQUE_KEY_FLAG : constant := 4;  --  /usr/include/mysql/mysql_com.h:80
   MULTIPLE_KEY_FLAG : constant := 8;  --  /usr/include/mysql/mysql_com.h:81
   BLOB_FLAG : constant := 16;  --  /usr/include/mysql/mysql_com.h:82
   UNSIGNED_FLAG : constant := 32;  --  /usr/include/mysql/mysql_com.h:83
   ZEROFILL_FLAG : constant := 64;  --  /usr/include/mysql/mysql_com.h:84
   BINARY_FLAG : constant := 128;  --  /usr/include/mysql/mysql_com.h:85

   ENUM_FLAG : constant := 256;  --  /usr/include/mysql/mysql_com.h:88
   AUTO_INCREMENT_FLAG : constant := 512;  --  /usr/include/mysql/mysql_com.h:89
   TIMESTAMP_FLAG : constant := 1024;  --  /usr/include/mysql/mysql_com.h:90
   SET_FLAG : constant := 2048;  --  /usr/include/mysql/mysql_com.h:91
   NO_DEFAULT_VALUE_FLAG : constant := 4096;  --  /usr/include/mysql/mysql_com.h:92
   NUM_FLAG : constant := 32768;  --  /usr/include/mysql/mysql_com.h:93
   PART_KEY_FLAG : constant := 16384;  --  /usr/include/mysql/mysql_com.h:94
   GROUP_FLAG : constant := 32768;  --  /usr/include/mysql/mysql_com.h:95
   UNIQUE_FLAG : constant := 65536;  --  /usr/include/mysql/mysql_com.h:96
   BINCMP_FLAG : constant := 131072;  --  /usr/include/mysql/mysql_com.h:97

   REFRESH_GRANT : constant := 1;  --  /usr/include/mysql/mysql_com.h:99
   REFRESH_LOG : constant := 2;  --  /usr/include/mysql/mysql_com.h:100
   REFRESH_TABLES : constant := 4;  --  /usr/include/mysql/mysql_com.h:101
   REFRESH_HOSTS : constant := 8;  --  /usr/include/mysql/mysql_com.h:102
   REFRESH_STATUS : constant := 16;  --  /usr/include/mysql/mysql_com.h:103
   REFRESH_THREADS : constant := 32;  --  /usr/include/mysql/mysql_com.h:104
   REFRESH_SLAVE : constant := 64;  --  /usr/include/mysql/mysql_com.h:105

   REFRESH_MASTER : constant := 128;  --  /usr/include/mysql/mysql_com.h:107

   REFRESH_READ_LOCK : constant := 16384;  --  /usr/include/mysql/mysql_com.h:111
   REFRESH_FAST : constant := 32768;  --  /usr/include/mysql/mysql_com.h:112

   REFRESH_QUERY_CACHE : constant := 65536;  --  /usr/include/mysql/mysql_com.h:115
   REFRESH_QUERY_CACHE_FREE : constant := 16#20000#;  --  /usr/include/mysql/mysql_com.h:116
   REFRESH_DES_KEY_FILE : constant := 16#40000#;  --  /usr/include/mysql/mysql_com.h:117
   REFRESH_USER_RESOURCES : constant := 16#80000#;  --  /usr/include/mysql/mysql_com.h:118

   CLIENT_LONG_PASSWORD : constant := 1;  --  /usr/include/mysql/mysql_com.h:120
   CLIENT_FOUND_ROWS : constant := 2;  --  /usr/include/mysql/mysql_com.h:121
   CLIENT_LONG_FLAG : constant := 4;  --  /usr/include/mysql/mysql_com.h:122
   CLIENT_CONNECT_WITH_DB : constant := 8;  --  /usr/include/mysql/mysql_com.h:123
   CLIENT_NO_SCHEMA : constant := 16;  --  /usr/include/mysql/mysql_com.h:124
   CLIENT_COMPRESS : constant := 32;  --  /usr/include/mysql/mysql_com.h:125
   CLIENT_ODBC : constant := 64;  --  /usr/include/mysql/mysql_com.h:126
   CLIENT_LOCAL_FILES : constant := 128;  --  /usr/include/mysql/mysql_com.h:127
   CLIENT_IGNORE_SPACE : constant := 256;  --  /usr/include/mysql/mysql_com.h:128
   CLIENT_PROTOCOL_41 : constant := 512;  --  /usr/include/mysql/mysql_com.h:129
   CLIENT_INTERACTIVE : constant := 1024;  --  /usr/include/mysql/mysql_com.h:130
   CLIENT_SSL : constant := 2048;  --  /usr/include/mysql/mysql_com.h:131
   CLIENT_IGNORE_SIGPIPE : constant := 4096;  --  /usr/include/mysql/mysql_com.h:132
   CLIENT_TRANSACTIONS : constant := 8192;  --  /usr/include/mysql/mysql_com.h:133
   CLIENT_RESERVED : constant := 16384;  --  /usr/include/mysql/mysql_com.h:134
   CLIENT_SECURE_CONNECTION : constant := 32768;  --  /usr/include/mysql/mysql_com.h:135
   CLIENT_MULTI_STATEMENTS : constant := (1 ** 16);  --  /usr/include/mysql/mysql_com.h:136
   CLIENT_MULTI_RESULTS : constant := (1 ** 17);  --  /usr/include/mysql/mysql_com.h:137

   CLIENT_SSL_VERIFY_SERVER_CERT : constant := (1 ** 30);  --  /usr/include/mysql/mysql_com.h:139
   CLIENT_REMEMBER_OPTIONS : constant := (1 ** 31);  --  /usr/include/mysql/mysql_com.h:140

   SERVER_STATUS_IN_TRANS : constant := 1;  --  /usr/include/mysql/mysql_com.h:142
   SERVER_STATUS_AUTOCOMMIT : constant := 2;  --  /usr/include/mysql/mysql_com.h:143
   SERVER_MORE_RESULTS_EXISTS : constant := 8;  --  /usr/include/mysql/mysql_com.h:144
   SERVER_QUERY_NO_GOOD_INDEX_USED : constant := 16;  --  /usr/include/mysql/mysql_com.h:145
   SERVER_QUERY_NO_INDEX_USED : constant := 32;  --  /usr/include/mysql/mysql_com.h:146

   SERVER_STATUS_CURSOR_EXISTS : constant := 64;  --  /usr/include/mysql/mysql_com.h:152

   SERVER_STATUS_LAST_ROW_SENT : constant := 128;  --  /usr/include/mysql/mysql_com.h:157
   SERVER_STATUS_DB_DROPPED : constant := 256;  --  /usr/include/mysql/mysql_com.h:158
   SERVER_STATUS_NO_BACKSLASH_ESCAPES : constant := 512;  --  /usr/include/mysql/mysql_com.h:159

   MYSQL_ERRMSG_SIZE : constant := 512;  --  /usr/include/mysql/mysql_com.h:161
   NET_READ_TIMEOUT : constant := 30;  --  /usr/include/mysql/mysql_com.h:162
   NET_WRITE_TIMEOUT : constant := 60;  --  /usr/include/mysql/mysql_com.h:163
   --  unsupported macro: NET_WAIT_TIMEOUT 8*60*60

   ONLY_KILL_QUERY : constant := 1;  --  /usr/include/mysql/mysql_com.h:166

   MAX_TINYINT_WIDTH : constant := 3;  --  /usr/include/mysql/mysql_com.h:171
   MAX_SMALLINT_WIDTH : constant := 5;  --  /usr/include/mysql/mysql_com.h:172
   MAX_MEDIUMINT_WIDTH : constant := 8;  --  /usr/include/mysql/mysql_com.h:173
   MAX_INT_WIDTH : constant := 10;  --  /usr/include/mysql/mysql_com.h:174
   MAX_BIGINT_WIDTH : constant := 20;  --  /usr/include/mysql/mysql_com.h:175
   MAX_CHAR_WIDTH : constant := 255;  --  /usr/include/mysql/mysql_com.h:176
   MAX_BLOB_WIDTH : constant := 8192;  --  /usr/include/mysql/mysql_com.h:177
   --  unsupported macro: packet_error (~(unsigned long) 0)
   --  unsupported macro: CLIENT_MULTI_QUERIES CLIENT_MULTI_STATEMENTS
   --  unsupported macro: FIELD_TYPE_DECIMAL MYSQL_TYPE_DECIMAL
   --  unsupported macro: FIELD_TYPE_NEWDECIMAL MYSQL_TYPE_NEWDECIMAL
   --  unsupported macro: FIELD_TYPE_TINY MYSQL_TYPE_TINY
   --  unsupported macro: FIELD_TYPE_SHORT MYSQL_TYPE_SHORT
   --  unsupported macro: FIELD_TYPE_LONG MYSQL_TYPE_LONG
   --  unsupported macro: FIELD_TYPE_FLOAT MYSQL_TYPE_FLOAT
   --  unsupported macro: FIELD_TYPE_DOUBLE MYSQL_TYPE_DOUBLE
   --  unsupported macro: FIELD_TYPE_NULL MYSQL_TYPE_NULL
   --  unsupported macro: FIELD_TYPE_TIMESTAMP MYSQL_TYPE_TIMESTAMP
   --  unsupported macro: FIELD_TYPE_LONGLONG MYSQL_TYPE_LONGLONG
   --  unsupported macro: FIELD_TYPE_INT24 MYSQL_TYPE_INT24
   --  unsupported macro: FIELD_TYPE_DATE MYSQL_TYPE_DATE
   --  unsupported macro: FIELD_TYPE_TIME MYSQL_TYPE_TIME
   --  unsupported macro: FIELD_TYPE_DATETIME MYSQL_TYPE_DATETIME
   --  unsupported macro: FIELD_TYPE_YEAR MYSQL_TYPE_YEAR
   --  unsupported macro: FIELD_TYPE_NEWDATE MYSQL_TYPE_NEWDATE
   --  unsupported macro: FIELD_TYPE_ENUM MYSQL_TYPE_ENUM
   --  unsupported macro: FIELD_TYPE_SET MYSQL_TYPE_SET
   --  unsupported macro: FIELD_TYPE_TINY_BLOB MYSQL_TYPE_TINY_BLOB
   --  unsupported macro: FIELD_TYPE_MEDIUM_BLOB MYSQL_TYPE_MEDIUM_BLOB
   --  unsupported macro: FIELD_TYPE_LONG_BLOB MYSQL_TYPE_LONG_BLOB
   --  unsupported macro: FIELD_TYPE_BLOB MYSQL_TYPE_BLOB
   --  unsupported macro: FIELD_TYPE_VAR_STRING MYSQL_TYPE_VAR_STRING
   --  unsupported macro: FIELD_TYPE_STRING MYSQL_TYPE_STRING
   --  unsupported macro: FIELD_TYPE_CHAR MYSQL_TYPE_TINY
   --  unsupported macro: FIELD_TYPE_INTERVAL MYSQL_TYPE_ENUM
   --  unsupported macro: FIELD_TYPE_GEOMETRY MYSQL_TYPE_GEOMETRY
   --  unsupported macro: FIELD_TYPE_BIT MYSQL_TYPE_BIT
   --  unsupported macro: MYSQL_SHUTDOWN_KILLABLE_CONNECT (unsigned char)(1 << 0)
   --  unsupported macro: MYSQL_SHUTDOWN_KILLABLE_TRANS (unsigned char)(1 << 1)
   --  unsupported macro: MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE (unsigned char)(1 << 2)
   --  unsupported macro: MYSQL_SHUTDOWN_KILLABLE_UPDATE (unsigned char)(1 << 3)
   --  arg-macro: function net_new_transaction (net)
   --    return (net).pkt_nr:=0;

   NET_HEADER_SIZE : constant := 4;  --  /usr/include/mysql/mysql_com.h:402
   COMP_HEADER_SIZE : constant := 3;  --  /usr/include/mysql/mysql_com.h:403
   --  unsupported macro: NULL_LENGTH ((unsigned long) ~0)

   MYSQL_STMT_HEADER : constant := 4;  --  /usr/include/mysql/mysql_com.h:464
   MYSQL_LONG_DATA_HEADER : constant := 6;  --  /usr/include/mysql/mysql_com.h:465

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

   --** Common definition between mysql server & client
   --

   --  USER_HOST_BUFF_SIZE -- length of string buffer, that is enough to contain
   --  username and hostname parts of the user identifier with trailing zero in
   --  MySQL standard format:
   --  user_name_part@host_name_part\0
   --

   --  You should add new commands to the end of this list, otherwise old
   --  servers won't be able to handle them as 'unsupported'.
   --

   subtype enum_server_command is unsigned;
   COM_SLEEP : constant enum_server_command := 0;
   COM_QUIT : constant enum_server_command := 1;
   COM_INIT_DB : constant enum_server_command := 2;
   COM_QUERY : constant enum_server_command := 3;
   COM_FIELD_LIST : constant enum_server_command := 4;
   COM_CREATE_DB : constant enum_server_command := 5;
   COM_DROP_DB : constant enum_server_command := 6;
   COM_REFRESH : constant enum_server_command := 7;
   COM_SHUTDOWN : constant enum_server_command := 8;
   COM_STATISTICS : constant enum_server_command := 9;
   COM_PROCESS_INFO : constant enum_server_command := 10;
   COM_CONNECT : constant enum_server_command := 11;
   COM_PROCESS_KILL : constant enum_server_command := 12;
   COM_DEBUG : constant enum_server_command := 13;
   COM_PING : constant enum_server_command := 14;
   COM_TIME : constant enum_server_command := 15;
   COM_DELAYED_INSERT : constant enum_server_command := 16;
   COM_CHANGE_USER : constant enum_server_command := 17;
   COM_BINLOG_DUMP : constant enum_server_command := 18;
   COM_TABLE_DUMP : constant enum_server_command := 19;
   COM_CONNECT_OUT : constant enum_server_command := 20;
   COM_REGISTER_SLAVE : constant enum_server_command := 21;
   COM_STMT_PREPARE : constant enum_server_command := 22;
   COM_STMT_EXECUTE : constant enum_server_command := 23;
   COM_STMT_SEND_LONG_DATA : constant enum_server_command := 24;
   COM_STMT_CLOSE : constant enum_server_command := 25;
   COM_STMT_RESET : constant enum_server_command := 26;
   COM_SET_OPTION : constant enum_server_command := 27;
   COM_STMT_FETCH : constant enum_server_command := 28;
   COM_END : constant enum_server_command := 29;  -- /usr/include/mysql/mysql_com.h:52:1

   --  don't forget to update const char *command_name[] in sql_parse.cc
   --  Must be last
   --  Length of random string sent by server on handshake; this is also length of
   --  obfuscated password, recieved from client
   --

   --  length of password stored in the db: new passwords are preceeded with '*'
   --  The following are only sent to new clients
   --  The following can't be set with mysql_refresh()
   --  RESET (remove all queries) from query cache
   --  The server was able to fulfill the clients request and opened a
   --  read-only non-scrollable cursor for a query. This flag comes
   --  in reply to COM_STMT_EXECUTE and COM_STMT_FETCH commands.
   --

   --  This flag is sent when a read-only cursor is exhausted, in reply to
   --  COM_STMT_FETCH command.
   --

   -- Only C
   --  skipped empty struct st_vio

   --  skipped empty struct Vio

   type anon1416_anon1442_array is array (0 .. 511) of aliased char;
   type anon1416_anon1443_array is array (0 .. 5) of aliased char;
   type st_net is record
      the_vio : System.Address;  -- /usr/include/mysql/mysql_com.h:181:8
      buff : access unsigned_char;  -- /usr/include/mysql/mysql_com.h:182:18
      buff_end : access unsigned_char;  -- /usr/include/mysql/mysql_com.h:182:24
      write_pos : access unsigned_char;  -- /usr/include/mysql/mysql_com.h:182:34
      read_pos : access unsigned_char;  -- /usr/include/mysql/mysql_com.h:182:45
      fd : aliased my_socket;  -- /usr/include/mysql/mysql_com.h:183:13
      max_packet : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:184:17
      max_packet_size : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:184:28
      pkt_nr : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:185:16
      compress_pkt_nr : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:185:23
      write_timeout : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:186:16
      read_timeout : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:186:31
      retry_count : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:186:45
      fcntl : aliased int;  -- /usr/include/mysql/mysql_com.h:187:7
      compress : aliased char;  -- /usr/include/mysql/mysql_com.h:188:11
      remain_in_buf : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:194:17
      length : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:194:31
      buf_length : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:194:39
      where_b : aliased unsigned_long;  -- /usr/include/mysql/mysql_com.h:194:51
      return_status : access unsigned;  -- /usr/include/mysql/mysql_com.h:195:17
      reading_or_writing : aliased unsigned_char;  -- /usr/include/mysql/mysql_com.h:196:17
      save_char : aliased char;  -- /usr/include/mysql/mysql_com.h:197:8
      no_send_ok : aliased char;  -- /usr/include/mysql/mysql_com.h:198:11
      no_send_eof : aliased char;  -- /usr/include/mysql/mysql_com.h:199:11
      no_send_error : aliased char;  -- /usr/include/mysql/mysql_com.h:204:11
      last_error : aliased anon1416_anon1442_array;  -- /usr/include/mysql/mysql_com.h:210:8
      sqlstate : aliased anon1416_anon1443_array;  -- /usr/include/mysql/mysql_com.h:210:39
      last_errno : aliased unsigned;  -- /usr/include/mysql/mysql_com.h:211:16
      error : aliased unsigned_char;  -- /usr/include/mysql/mysql_com.h:212:17
      query_cache_query : Interfaces.C.Strings.chars_ptr;  --  mysql_mysql_h.gptr;  -- /usr/include/mysql/mysql_com.h:218:8
      report_error : aliased char;  -- /usr/include/mysql/mysql_com.h:220:11
      return_errno : aliased char;  -- /usr/include/mysql/mysql_com.h:221:11
   end record;
   pragma Convention (C, st_net);  -- /usr/include/mysql/mysql_com.h:179:16

   --  For Perl DBI/dbd
   --    The following variable is set if we are doing several queries in one
   --    command ( as in LOAD TABLE ... FROM MASTER ),
   --    and do not want to confuse the client with OK at the wrong time
   --

   --  For SPs and other things that do multiple stmts
   --  For SPs' first version read-only cursors
   --    Set if OK packet is already sent, and we do not need to send error
   --    messages
   --

   --    Pointer to query object in query cache, do not equal NULL (0) for
   --    queries in cache that have not stored its results yet
   --

   --    'query_cache_query' should be accessed only via query cache
   --    functions and methods to maintain proper locking.
   --

   -- We should report error (we have unreported error)
   subtype NET is st_net;

   subtype enum_field_types is unsigned;
   MYSQL_TYPE_DECIMAL : constant enum_field_types := 0;
   MYSQL_TYPE_TINY : constant enum_field_types := 1;
   MYSQL_TYPE_SHORT : constant enum_field_types := 2;
   MYSQL_TYPE_LONG : constant enum_field_types := 3;
   MYSQL_TYPE_FLOAT : constant enum_field_types := 4;
   MYSQL_TYPE_DOUBLE : constant enum_field_types := 5;
   MYSQL_TYPE_NULL : constant enum_field_types := 6;
   MYSQL_TYPE_TIMESTAMP : constant enum_field_types := 7;
   MYSQL_TYPE_LONGLONG : constant enum_field_types := 8;
   MYSQL_TYPE_INT24 : constant enum_field_types := 9;
   MYSQL_TYPE_DATE : constant enum_field_types := 10;
   MYSQL_TYPE_TIME : constant enum_field_types := 11;
   MYSQL_TYPE_DATETIME : constant enum_field_types := 12;
   MYSQL_TYPE_YEAR : constant enum_field_types := 13;
   MYSQL_TYPE_NEWDATE : constant enum_field_types := 14;
   MYSQL_TYPE_VARCHAR : constant enum_field_types := 15;
   MYSQL_TYPE_BIT : constant enum_field_types := 16;
   MYSQL_TYPE_NEWDECIMAL : constant enum_field_types := 246;
   MYSQL_TYPE_ENUM : constant enum_field_types := 247;
   MYSQL_TYPE_SET : constant enum_field_types := 248;
   MYSQL_TYPE_TINY_BLOB : constant enum_field_types := 249;
   MYSQL_TYPE_MEDIUM_BLOB : constant enum_field_types := 250;
   MYSQL_TYPE_LONG_BLOB : constant enum_field_types := 251;
   MYSQL_TYPE_BLOB : constant enum_field_types := 252;
   MYSQL_TYPE_VAR_STRING : constant enum_field_types := 253;
   MYSQL_TYPE_STRING : constant enum_field_types := 254;
   MYSQL_TYPE_GEOMETRY : constant enum_field_types := 255;  -- /usr/include/mysql/mysql_com.h:226:6

   -- For backward compatibility
   -- Shutdown/kill enums and constants
   -- Bits for THD::killable.
   subtype mysql_enum_shutdown_level is unsigned;
   SHUTDOWN_DEFAULT : constant mysql_enum_shutdown_level := 0;
   SHUTDOWN_WAIT_CONNECTIONS : constant mysql_enum_shutdown_level := 1;
   SHUTDOWN_WAIT_TRANSACTIONS : constant mysql_enum_shutdown_level := 2;
   SHUTDOWN_WAIT_UPDATES : constant mysql_enum_shutdown_level := 8;
   SHUTDOWN_WAIT_ALL_BUFFERS : constant mysql_enum_shutdown_level := 16;
   SHUTDOWN_WAIT_CRITICAL_BUFFERS : constant mysql_enum_shutdown_level := 17;
   KILL_QUERY : constant mysql_enum_shutdown_level := 254;
   KILL_CONNECTION : constant mysql_enum_shutdown_level := 255;  -- /usr/include/mysql/mysql_com.h:288:6

   --    We want levels to be in growing order of hardness (because we use number
   --    comparisons). Note that DEFAULT does not respect the growing property, but
   --    it's ok.
   --

   -- wait for existing connections to finish
   -- wait for existing trans to finish
   -- wait for existing updates to finish (=> no partial MyISAM update)
   -- flush InnoDB buffers and other storage engines' buffers
   -- don't flush InnoDB buffers, flush other storage engines' buffers
   -- Now the 2 levels of the KILL command
   subtype enum_cursor_type is unsigned;
   CURSOR_TYPE_NO_CURSOR : constant enum_cursor_type := 0;
   CURSOR_TYPE_READ_ONLY : constant enum_cursor_type := 1;
   CURSOR_TYPE_FOR_UPDATE : constant enum_cursor_type := 2;
   CURSOR_TYPE_SCROLLABLE : constant enum_cursor_type := 4;  -- /usr/include/mysql/mysql_com.h:314:1

   -- options for mysql_set_option
   subtype enum_mysql_set_option is unsigned;
   MYSQL_OPTION_MULTI_STATEMENTS_ON : constant enum_mysql_set_option := 0;
   MYSQL_OPTION_MULTI_STATEMENTS_OFF : constant enum_mysql_set_option := 1;  -- /usr/include/mysql/mysql_com.h:324:1

   function my_net_init (arg1 : access st_net; arg2 : System.Address) return char;  -- /usr/include/mysql/mysql_com.h:335:9
   pragma Import (C, my_net_init, "my_net_init");

   procedure my_net_local_init (arg1 : access st_net);  -- /usr/include/mysql/mysql_com.h:336:6
   pragma Import (C, my_net_local_init, "my_net_local_init");

   procedure net_end (arg1 : access st_net);  -- /usr/include/mysql/mysql_com.h:337:6
   pragma Import (C, net_end, "net_end");

   procedure net_clear (arg1 : access st_net);  -- /usr/include/mysql/mysql_com.h:338:6
   pragma Import (C, net_clear, "net_clear");

   function net_realloc (arg1 : access st_net; arg2 : unsigned_long) return char;  -- /usr/include/mysql/mysql_com.h:339:9
   pragma Import (C, net_realloc, "net_realloc");

   function net_flush (arg1 : access st_net) return char;  -- /usr/include/mysql/mysql_com.h:340:9
   pragma Import (C, net_flush, "net_flush");

   function my_net_write
     (arg1 : access st_net;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return char;  -- /usr/include/mysql/mysql_com.h:341:9
   pragma Import (C, my_net_write, "my_net_write");

   function net_write_command
     (arg1 : access st_net;
      arg2 : unsigned_char;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : unsigned_long;
      arg5 : Interfaces.C.Strings.chars_ptr;
      arg6 : unsigned_long) return char;  -- /usr/include/mysql/mysql_com.h:342:9
   pragma Import (C, net_write_command, "net_write_command");

   function net_real_write
     (arg1 : access st_net;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned_long) return int;  -- /usr/include/mysql/mysql_com.h:345:5
   pragma Import (C, net_real_write, "net_real_write");

   function my_net_read (arg1 : access st_net) return unsigned_long;  -- /usr/include/mysql/mysql_com.h:346:15
   pragma Import (C, my_net_read, "my_net_read");

   --  The following function is not meant for normal usage
   --  Currently it's used internally by manager.c
   --

   --  skipped empty struct sockaddr

   function my_connect
     (arg1 : my_socket;
      arg2 : System.Address;
      arg3 : unsigned;
      arg4 : unsigned) return int;  -- /usr/include/mysql/mysql_com.h:358:5
   pragma Import (C, my_connect, "my_connect");

   type rand_struct is record
      seed1 : aliased unsigned_long;
      seed2 : aliased unsigned_long;
      max_value : aliased unsigned_long;
      max_value_dbl : aliased double;
   end record;
   pragma Convention (C, rand_struct);

   -- The following is for user defined functions
   subtype Item_result is unsigned;
   STRING_RESULT : constant Item_result := 0;
   REAL_RESULT : constant Item_result := 1;
   INT_RESULT : constant Item_result := 2;
   ROW_RESULT : constant Item_result := 3;
   DECIMAL_RESULT : constant Item_result := 4;

   -- Number of arguments
   type st_udf_args is record
      arg_count : aliased unsigned;
      arg_type : access Item_result;
      args : System.Address;
      lengths : access unsigned_long;
      maybe_null : Interfaces.C.Strings.chars_ptr;
      attributes : System.Address;
      attribute_lengths : access unsigned_long;
   end record;
   pragma Convention (C, st_udf_args);

   -- Pointer to item_results
   -- Pointer to argument
   -- Length of string arguments
   -- Set to 1 for all maybe_null args
   -- Pointer to attribute name
   -- Length of attribute arguments
   subtype UDF_ARGS is st_udf_args;

   -- This holds information about the result
   -- 1 if function can return NULL
   type st_udf_init is record
      maybe_null : aliased char;
      decimals : aliased unsigned;
      max_length : aliased unsigned_long;
      ptr : Interfaces.C.Strings.chars_ptr;
      const_item : aliased char;
   end record;
   pragma Convention (C, st_udf_init);

   --  for real functions
   --  For string functions
   --  free pointer for function data
   --  1 if function always returns the same value
   subtype UDF_INIT is st_udf_init;

   --
   --  TODO: add a notion for determinism of the UDF.
   --  See Item_udf_func::update_used_tables ()
   --

   --  Constants when using compression
   --  Prototypes to password functions
   --  These functions are used for authentication by client and server and
   --  implemented in sql/password.c
   --

   procedure randominit
     (arg1 : access rand_struct;
      arg2 : unsigned_long;
      arg3 : unsigned_long);
   pragma Import (C, randominit, "randominit");

   function my_rnd (arg1 : access rand_struct) return double;
   pragma Import (C, my_rnd, "my_rnd");

   procedure create_random_string
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : unsigned;
      arg3 : access rand_struct);
   pragma Import (C, create_random_string, "create_random_string");

   procedure hash_password
     (arg1 : access unsigned_long;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned);
   pragma Import (C, hash_password, "hash_password");

   procedure Make_Scrambled_Password_323 (Arg1 : Interfaces.C.Strings.chars_ptr;
                                          arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, make_scrambled_password_323, "make_scrambled_password_323");

   procedure scramble_323
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, scramble_323, "scramble_323");

   function check_scramble_323
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : access unsigned_long) return char;
   pragma Import (C, check_scramble_323, "check_scramble_323");

   procedure Get_Salt_From_Password_323 (Arg1 : access Unsigned_Long;
                                         arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, get_salt_from_password_323, "get_salt_from_password_323");

   procedure Make_Password_From_Salt_323 (Arg1 : Interfaces.C.Strings.chars_ptr;
                                          arg2 : access unsigned_long);
   pragma Import (C, make_password_from_salt_323, "make_password_from_salt_323");

   procedure Make_Scrambled_Password (Arg1 : Interfaces.C.Strings.chars_ptr;
                                      arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, make_scrambled_password, "make_scrambled_password");

   procedure scramble
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, scramble, "scramble");

   function check_scramble
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : access unsigned_char) return char;
   pragma Import (C, check_scramble, "check_scramble");

   procedure Get_Salt_From_Password (Arg1 : access Unsigned_Char;
                                     arg2 : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, get_salt_from_password, "get_salt_from_password");

   procedure Make_Password_From_Salt (Arg1 : Interfaces.C.Strings.chars_ptr;
                                      arg2 : access unsigned_char);
   pragma Import (C, make_password_from_salt, "make_password_from_salt");

   function octet2hex
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, octet2hex, "octet2hex");

   -- end of password.c
   function Get_Tty_Password (Arg1 : Interfaces.C.Strings.chars_ptr)
                              return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, get_tty_password, "get_tty_password");

   function mysql_errno_to_sqlstate (arg1 : unsigned) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, mysql_errno_to_sqlstate, "mysql_errno_to_sqlstate");

   -- Some other useful functions
   function my_init return char;
   pragma Import (C, my_init, "my_init");

   function modify_defaults_file
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : int) return int;
   pragma Import (C, modify_defaults_file, "modify_defaults_file");

   function load_defaults
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : access int;
      arg4 : System.Address) return int;
   pragma Import (C, load_defaults, "load_defaults");

   function my_thread_init return char;
   pragma Import (C, my_thread_init, "my_thread_init");

   procedure my_thread_end;
   pragma Import (C, my_thread_end, "my_thread_end");

end Mysql.Com ;
