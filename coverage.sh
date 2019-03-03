#!/bin/sh
lcov --base-directory . --directory . -c -o ado.cov >/dev/null
bin/ado_harness -p SQLite -t 120 -xml ado-sqlite-aunit.xml -config test-sqlite.properties
bin/ado_harness -p MySQL -xml ado-mysql-aunit.xml -config test-mysql.properties
bin/ado_harness -p Postgresql -xml ado-postgresql-aunit.xml -config test-postgresql.properties
(lcov --base-directory . --directory . -c -o ado.cov
lcov --remove ado.cov "/usr*" -o ado.cov 
lcov --remove ado.cov "/opt*" -o ado.cov 
lcov --remove ado.cov "regtests*" -o ado.cov
lcov --remove ado.cov "*/ada-util/*" -o ado.cov
lcov --remove ado.cov ada-ado/b__ado_harness.adb -o ado.cov ) >/dev/null
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 ado.cov
 
