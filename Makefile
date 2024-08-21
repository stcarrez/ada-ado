NAME=ado
VERSION=2.4.1

DIST_DIR=ada-ado-$(VERSION)
DIST_FILE=ada-ado-$(VERSION).tar.gz

MAKE_ARGS += -XADO_BUILD=$(BUILD)

-include Makefile.conf

SQLITE?=$(shell which sqlite3)
MYSQL?=$(shell which mysql)
PSQL=$(shell which psql)

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
build-test::  lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

lib-setup:: regtests/src/ado-testsuite-drivers.adb

regtests/src/ado-testsuite-drivers.adb: regtests/src/ado-testsuite-drivers.gpb
	gnatprep -DHAVE_MYSQL=$(GNAT_MYSQL_VAR) \
	         -DHAVE_SQLITE=$(GNAT_SQLITE_VAR) \
	         -DHAVE_POSTGRESQL=$(GNAT_POSTGRESQL_VAR) \
		 regtests/src/ado-testsuite-drivers.gpb $@

# Build and run the unit tests
test:	test-sqlite test-mysql test-postgresql

test-sqlite:		build regtests.db
ifneq (, ${SQLITE})
	bin/ado_harness -l $(NAME):SQLite: -p SQLite -t 120 -xml ado-sqlite-aunit.xml -config test-sqlite.properties
endif

test-mysql:		build create-mysql-tests
ifneq (, ${MYSQL})
	bin/ado_harness -l $(NAME):MySQL: -p MySQL -xml ado-mysql-aunit.xml -config test-mysql.properties
endif

test-postgresql:	build create-postgresql-tests
ifneq (, ${PSQL})
	bin/ado_harness -l $(NAME):Postgresql: -p Postgresql -xml ado-postgresql-aunit.xml -config test-postgresql.properties
endif

CLEAN_FILES=drivers/src/ado-drivers-initialize.adb mysql/src/mysql-lib.ads regtests/src/ado-testsuite-drivers.adb

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
ifneq (, ${SQLITE})
	$(SQLITE) $@ < db/regtests/sqlite/create-ado-sqlite.sql
endif

# Create the samples sqlite database
samples.db:
ifneq (, ${SQLITE})
	$(SQLITE) $@ < db/samples/sqlite/create-ado-sqlite.sql
endif

# Create the tables in the database.
# (The database itself must have been created)
create-mysql-tests:
ifneq (, ${MYSQL})
	$(MYSQL) --defaults-extra-file=db/regtests/mysql/mysql.cnf < db/regtests/mysql/drop-ado-mysql.sql
	$(MYSQL) --defaults-extra-file=db/regtests/mysql/mysql.cnf < db/regtests/mysql/create-ado-mysql.sql
endif

create-postgresql-tests:
ifneq (, ${PSQL})
	$(PSQL) -q "postgresql://localhost:5432/ado_test?user=ado&password=ado" --file=db/regtests/postgresql/drop-ado-postgresql.sql
	$(PSQL) -q "postgresql://localhost:5432/ado_test?user=ado&password=ado" --file=db/regtests/postgresql/create-ado-postgresql.sql
endif

install:: install-data

install-data::
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db
	${CP} db/*.xml $(DESTDIR)${dynamodir}/ado/db/
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/mysql
	${CP} db/mysql/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/mysql
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/postgresql
	${CP} db/postgresql/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/postgresql
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/sqlite
	${CP} db/sqlite/ado-*.sql $(DESTDIR)${dynamodir}/ado/db/sqlite
	${CP} dynamo.xml $(DESTDIR)${dynamodir}/ado/
	${CP} NOTICE.txt $(DESTDIR)${dynamodir}/ado/
	${CP} LICENSE.txt $(DESTDIR)${dynamodir}/ado/
	${MKDIR} -p $(DESTDIR)${dynamodir}/ado/db/migrate
	(cd db/migrate && tar --exclude='*~' -cf - . ) | (cd $(DESTDIR)${dynamodir}/ado/db/migrate && tar xf -)

uninstall::
	rm -rf $(DESTDIR)${dynamodir}/ado

.PHONY: doc

$(eval $(call ada_library,$(NAME),.))
$(eval $(call ada_library,ado_sqlite,sqlite))
$(eval $(call ada_library,ado_mysql,mysql))
$(eval $(call ada_library,ado_postgresql,postgresql))

$(eval $(call ada_library,ado_all,drivers))
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
