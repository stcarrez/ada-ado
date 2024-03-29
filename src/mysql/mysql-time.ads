
with Interfaces.C; use Interfaces.C;
with Mysql.Mysql;  use Mysql.Mysql;

package Mysql.Time is
   pragma Preelaborate;

   --  Copyright (C) 2004 MySQL AB
   --  This program is free software; you can redistribute it and/or modify
   --  it under the terms of the GNU General Public License as published by
   --  the Free Software Foundation; version 2 of the License.
   --  This program is distributed in the hope that it will be useful,
   --  but WITHOUT ANY WARRANTY; without even the implied warranty of
   --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   --  GNU General Public License for more details.
   --  You should have received a copy of the GNU General Public License
   --  along with this program; if not, write to the Free Software
   --  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   --  Time declarations shared between the server and client API:
   --  you should not add anything to this header unless it's used
   --  (and hence should be visible) in mysql.h.
   --  If you're looking for a place to add new time-related declaration,
   --  it's most likely my_time.h. See also "C API Handling of Date
   --  and Time Values" chapter in documentation.
   --

   subtype enum_mysql_timestamp_type is unsigned;
   MYSQL_TIMESTAMP_NONE : constant enum_mysql_timestamp_type := 16#fffffffe#;
   MYSQL_TIMESTAMP_ERROR : constant enum_mysql_timestamp_type := 16#ffffffff#;
   MYSQL_TIMESTAMP_DATE : constant enum_mysql_timestamp_type := 0;
   MYSQL_TIMESTAMP_DATETIME : constant enum_mysql_timestamp_type := 1;
   MYSQL_TIMESTAMP_TIME : constant enum_mysql_timestamp_type := 2;

   --  Structure which is used to represent datetime values inside MySQL.
   --  We assume that values in this structure are normalized, i.e. year <= 9999,
   --  month <= 12, day <= 31, hour <= 23, hour <= 59, hour <= 59. Many functions
   --  in server such as my_system_gmt_sec() or make_time() family of functions
   --  rely on this (actually now usage of make_*() family relies on a bit weaker
   --  restriction). Also functions that produce MYSQL_TIME as result ensure this.
   --  There is one exception to this rule though if this structure holds time
   --  value (time_type == MYSQL_TIMESTAMP_TIME) days and hour member can hold
   --  bigger values.
   --

   type st_mysql_time is record
      year : aliased unsigned;
      month : aliased unsigned;
      day : aliased unsigned;
      hour : aliased unsigned;
      minute : aliased unsigned;
      second : aliased unsigned;
      second_part : aliased unsigned_long;
      neg : aliased my_bool;
      time_type : aliased enum_mysql_timestamp_type;
   end record;
   pragma Convention (C, st_mysql_time);

   subtype MYSQL_TIME is st_mysql_time;

end Mysql.Time;
