#!/bin/sh
PKG_CONFIG=`which pkg-config`
if test "T$PKG_CONFIG" != T; then
  MYSQL_LIBS=`pkg-config --libs mariadb`
fi
if test "T$MYSQL_LIBS" = T; then
  MYSQL_LIBS='-lmariadb'
fi
libs=`echo \""$MYSQL_LIBS"\" | sed -e 's,^ *,,' -e 's,  , ,g' -e 's, $,,g' | sed -e 's, ," \& ASCII.NUL \& ",g'`
libs=`echo "$libs" | sed -e 's,ASCII.NUL & "" & ASCII.NUL,ASCII.NUL,g'`
gnatprep -DMYSQL_LIB="$libs" src/mysql-lib.gpb src/mysql-lib.ads

