NAME=ado
VERSION=2.4.1

DIST_DIR=ada-ado-$(VERSION)
DIST_FILE=ada-ado-$(VERSION).tar.gz

MAKE_ARGS += -XADO_BUILD=$(BUILD)

-include Makefile.conf

SQLITE?=$(shell which sqlite3)
SQLCIPHER?=$(shell which sqlcipher)
MYSQL?=$(shell which mysql)
PSQL=$(shell which psql)

include Makefile.defaults

HAVE_SQLITE?=yes
HAVE_SQLCIPHER?=yes
HAVE_MYSQL?=yes
HAVE_POSTGRESQL?=yes

# Force HAVE_SQLITE if SQLCipher is selected.
ifeq ($(HAVE_SQLCIPHER),yes)
HAVE_SQLITE=yes

MAKE_ARGS += -XADO_USE_SQLCIPHER=yes
endif

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XADO_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XADO_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

DEFAULT_ADA_PROJECT_PATH=$(SRC_ROOT)

ifeq ($(HAVE_SQLITE),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/sqlite
endif

ifeq ($(HAVE_MYSQL),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/mysql
endif

ifeq ($(HAVE_POSTGRESQL),yes)
DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/postgresql
endif

DEFAULT_ADA_PROJECT_PATH:=$(DEFAULT_ADA_PROJECT_PATH):$(SRC_ROOT)/drivers:$(ADA_PROJECT_PATH)

# Build executables for all mains defined by the project.
build-test::  lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

lib-setup::

# Build and run the unit tests

ifeq ($(HAVE_SQLITE),yes)
test::  test-sqlite

test-sqlite:		build regtests.db
	bin/ado_harness -l $(NAME):SQLite: -p SQLite -t 120 -xml ado-sqlite-aunit.xml -config test-sqlite.properties
endif

ifeq ($(HAVE_SQLCIPHER),yes)
test::  test-sqlcipher

test-sqlcipher:		build regtests.cipher
	bin/ado_harness -v -l $(NAME):SQLCipher: -p SQLCipher -t 120 -xml ado-sqlite-aunit.xml -config test-sqlcipher.properties
endif

ifeq ($(HAVE_MYSQL),yes)
test:: test-mysql

test-mysql:		build create-mysql-tests
	bin/ado_harness -l $(NAME):MySQL: -p MySQL -xml ado-mysql-aunit.xml -config test-mysql.properties
endif

ifeq ($(HAVE_POSTGRESQL),yes)
test:: test-postgresql

test-postgresql:	build create-postgresql-tests
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
  ORM_Tutorial.md \
  pagebreak.tex \
  ADO_Sessions.md \
  ADO_Sequences.md \
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

regtests.cipher:
ifneq (, ${SQLCIPHER})
	(echo "pragma key='test-password';"; cat db/regtests/sqlite/create-ado-sqlite.sql) | $(SQLCIPHER) $@
endif

# Create the samples sqlite database
samples.db:
ifneq (, ${SQLITE})
	$(SQLITE) $@ < db/samples/sqlite/create-ado-sqlite.sql
endif

samples.cipher:
ifneq (, ${SQLCIPHER})
	(echo "pragma key='samples-password';"; cat db/samples/sqlite/create-ado-sqlite.sql) | $(SQLCIPHER) $@
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

lib-setup::
ifneq ($(HAVE_ALIRE),yes)
ifeq ($(HAVE_MYSQL),yes)
	cd mysql && sh ./alire-setup.sh
endif
endif
	cd drivers && sh ./alire-setup.sh sqlite=$(HAVE_SQLITE) mysql=$(HAVE_MYSQL) postgresql=$(HAVE_POSTGRESQL)

$(eval $(call ada_library,$(NAME),.))
ifeq ($(HAVE_SQLITE),yes)
$(eval $(call ada_library,ado_sqlite,sqlite))
endif
ifeq ($(HAVE_MYSQL),yes)
$(eval $(call ada_library,ado_mysql,mysql))
endif
ifeq ($(HAVE_POSTGRESQL),yes)
$(eval $(call ada_library,ado_postgresql,postgresql))
endif

$(eval $(call ada_library,ado_all,drivers))
$(eval $(call alire_publish,.,ad/ado,ado-$(VERSION).toml))
ifeq ($(HAVE_POSTGRESQL),yes)
$(eval $(call alire_publish,postgresql,ad/ado_postgresql,ado_postgresql-$(VERSION).toml))
endif
ifeq ($(HAVE_MYSQL),yes)
$(eval $(call alire_publish,mysql,ad/ado_mysql,ado_mysql-$(VERSION).toml))
endif
ifeq ($(HAVE_SQLITE),yes)
$(eval $(call alire_publish,sqlite,ad/ado_sqlite,ado_sqlite-$(VERSION).toml))
endif
$(eval $(call alire_publish,drivers,ad/ado_all,ado_all-$(VERSION).toml))

setup::
	echo "HAVE_SQLITE=$(HAVE_SQLITE)" >> Makefile.conf
	echo "HAVE_SQLCIPHER=$(HAVE_SQLCIPHER)" >> Makefile.conf
	echo "HAVE_MYSQL=$(HAVE_MYSQL)" >> Makefile.conf
	echo "HAVE_POSTGRESQL=$(HAVE_POSTGRESQL)" >> Makefile.conf
