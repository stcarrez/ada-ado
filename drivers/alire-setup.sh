#!/bin/sh
(
 test "T$ADO_SQLITE_ALIRE_PREFIX" != T && echo HAVE_SQLITE:=True || echo HAVE_SQLITE:=False

 test "T$ADO_MYSQL_ALIRE_PREFIX" != T && echo HAVE_MYSQL:=True || echo HAVE_MYSQL:=False

 test "T$ADO_POSTGRESQL_ALIRE_PREFIX" != T && echo HAVE_POSTGRESQL:=True || echo HAVE_POSTGRESQL:=False
) > ado_all-new.def
if test -f ado_all.def && cmp ado_all-new.def ado_all.def; then
  rm -f ado_all-new.def
  exit 0
fi
mv ado_all-new.def ado_all.def
gnatprep src/ado-drivers-initialize.gpb src/ado-drivers-initialize.adb ado_all.def
gnatprep ado_all.gpg ado_all.gpr ado_all.def

