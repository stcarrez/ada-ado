NAME=ado

-include Makefile.conf

ifeq ($(HAVE_SQLITE),yes)
GNAT_SQLITE_VAR=True
else
GNAT_SQLITE_VAR=False
endif

ifeq ($(HAVE_MYSQL),yes)
GNAT_MYSQL_VAR=True
else
GNAT_MYSQL_VAR=False
endif

ifeq ($(HAVE_POSTGRESQL),yes)
GNAT_POSTGRESQL_VAR=True
else
GNAT_POSTGRESQL_VAR=False
endif

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XADO_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XADO_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::  setup
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS) -largs -Llib/$(NAME)/static

setup:: src/mysql/mysql-lib.ads regtests/ado-testsuite-drivers.adb src/drivers/ado-drivers-initialize.adb

# Configure the driver file
src/drivers/ado-drivers-initialize.adb: src/drivers/ado-drivers-initialize.gpb Makefile.conf
	gnatprep -DHAVE_MYSQL=$(GNAT_MYSQL_VAR) \
	          -DHAVE_SQLITE=$(GNAT_SQLITE_VAR) \
	          -DHAVE_POSTGRESQL=$(GNAT_POSTGRESQL_VAR) \
		  src/drivers/ado-drivers-initialize.gpb $@

src/mysql/mysql-lib.ads: src/mysql/mysql-lib.gpb Makefile.conf
	libs=`echo '"$(MYSQL_LIBS)"' | sed -e 's,^ *,,' -e 's,  , ,g' -e 's, $$,,g' | sed -e 's, ," \& ASCII.NUL \& ",g'` ; \
    libs=`echo "$$libs" | sed -e 's,ASCII.NUL & "" & ASCII.NUL,ASCII.NUL,g'`;\
	gnatprep -DMYSQL_LIB="$$libs" src/mysql/mysql-lib.gpb $@

regtests/ado-testsuite-drivers.adb: regtests/ado-testsuite-drivers.gpb Makefile.conf
	gnatprep -DHAVE_MYSQL=$(GNAT_MYSQL_VAR) \
	         -DHAVE_SQLITE=$(GNAT_SQLITE_VAR) \
	         -DHAVE_POSTGRESQL=$(GNAT_POSTGRESQL_VAR) \
		 regtests/ado-testsuite-drivers.gpb $@

# Build and run the unit tests
test:	test-sqlite test-mysql test-postgresql

test-sqlite:		build regtests.db
ifeq ($(HAVE_SQLITE),yes)
	bin/ado_harness -l $(NAME):SQLite: -p SQLite -t 120 -xml ado-sqlite-aunit.xml -config test-sqlite.properties
endif

test-mysql:		build create-mysql-tests
ifeq ($(HAVE_MYSQL),yes)
	bin/ado_harness -l $(NAME):MySQL: -p MySQL -xml ado-mysql-aunit.xml -config test-mysql.properties
endif

test-postgresql:	build create-postgresql-tests
ifeq ($(HAVE_POSTGRESQL),yes)
	bin/ado_harness -l $(NAME):Postgresql: -p Postgresql -xml ado-postgresql-aunit.xml -config test-postgresql.properties
endif

CLEAN_FILES=src/drivers/ado-drivers-initialize.adb src/mysql/mysql-lib.ads regtests/ado-testsuite-drivers.adb

ADO_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Tutorial.md \
  pagebreak.tex \
  ADO_Sessions.md \
  pagebreak.tex \
  ADO_Statements.md \
  pagebreak.tex \
  ADO_Model.md \
  pagebreak.tex \
  Migration.md \
  pagebreak.tex \
  Debugging.md

DOC_OPTIONS=-f markdown --listings --number-sections --toc
HTML_OPTIONS=-f markdown --listings --number-sections --toc --css pandoc.css

$(eval $(call pandoc_build,ado-book,$(ADO_DOC),\
	rm -f docs/user-list.md docs/alloc-sequence.md docs/user_hbm.md docs/version.md; \
	cat docs/Model.md docs/ADO_Objects.md > docs/ADO_Model.md))

generate:
	$(DYNAMO) generate db/regtests
	$(DYNAMO) generate db/samples

# Create the test sqlite database
regtests.db:
ifeq ($(HAVE_SQLITE),yes)
	sqlite3 $@ < db/regtests/sqlite/create-ado-sqlite.sql
endif

# Create the samples sqlite database
samples.db:
ifeq ($(HAVE_SQLITE),yes)
	sqlite3 $@ < db/samples/sqlite/create-ado-sqlite.sql
endif

# Create the tables in the database.
# (The database itself must have been created)
create-mysql-tests:
ifeq ($(HAVE_MYSQL),yes)
	mysql --defaults-extra-file=db/regtests/mysql/mysql.cnf < db/regtests/mysql/drop-ado-mysql.sql
	mysql --defaults-extra-file=db/regtests/mysql/mysql.cnf < db/regtests/mysql/create-ado-mysql.sql
endif

create-postgresql-tests:
ifeq ($(HAVE_POSTGRESQL),yes)
	psql -q "postgresql://localhost:5432/ado_test?user=ado&password=ado" --file=db/regtests/postgresql/drop-ado-postgresql.sql
	psql -q "postgresql://localhost:5432/ado_test?user=ado&password=ado" --file=db/regtests/postgresql/create-ado-postgresql.sql
endif

install:: install-data

install-data::
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db
	${CP} db/*.xml $(DESTDIR)${dynamodir}/ado/db/
ifeq ($(HAVE_MYSQL),yes)
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/mysql
	${CP} db/mysql/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/mysql
endif
ifeq ($(HAVE_POSTGRESQL),yes)
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/postgresql
	${CP} db/postgresql/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/postgresql
endif
ifeq ($(HAVE_SQLITE),yes)
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/sqlite
	${CP} db/sqlite/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/sqlite
endif
	${CP} dynamo.xml $(DESTDIR)${dynamodir}/ado/
	${CP} NOTICE.txt $(DESTDIR)${dynamodir}/ado/
	${CP} LICENSE.txt $(DESTDIR)${dynamodir}/ado/
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/migrate
	(cd db/migrate && tar --exclude='*~' -cf - . ) | (cd $(DESTDIR)${dynamodir}/ado/db/migrate && tar xf -)

uninstall::
	rm -rf $(DESTDIR)${dynamodir}/ado

.PHONY: doc

$(eval $(call ada_library,$(NAME)))

ifeq ($(HAVE_SQLITE),yes)
$(eval $(call ada_library,ado_sqlite))
endif

ifeq ($(HAVE_MYSQL),yes)
$(eval $(call ada_library,ado_mysql))
endif

ifeq ($(HAVE_POSTGRESQL),yes)
$(eval $(call ada_library,ado_postgresql))
endif

$(eval $(call ada_library,ado_all))
$(eval $(call alire_publish,.,ad/ado,ado-$(VERSION).toml))
ifeq ($(HAVE_POSTGRESQL),yes)
$(eval $(call alire_publish,.alire/postgresql,ad/ado_postgresql,ado_postgresql-$(VERSION).toml))
endif
ifeq ($(HAVE_MYSQL),yes)
$(eval $(call alire_publish,.alire/mysql,ad/ado_mysql,ado_mysql-$(VERSION).toml))
endif
ifeq ($(HAVE_SQLITE),yes)
$(eval $(call alire_publish,.alire/sqlite,ad/ado_sqlite,ado_sqlite-$(VERSION).toml))
endif
$(eval $(call alire_publish,.alire/all,ad/ado_all,ado_all-$(VERSION).toml))
