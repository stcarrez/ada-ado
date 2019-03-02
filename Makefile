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
build-test:  setup
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS) -largs -Llib/$(NAME)/static

setup:: src/mysql/mysql-lib.ads regtests/ado-testsuite-drivers.adb src/static/ado-drivers-initialize.adb

# Configure the driver file
src/static/ado-drivers-initialize.adb: src/static/ado-drivers-initialize.gpb Makefile.conf
	gnatprep -DHAVE_MYSQL=$(GNAT_MYSQL_VAR) \
	          -DHAVE_SQLITE=$(GNAT_SQLITE_VAR) \
	          -DHAVE_POSTGRESQL=$(GNAT_POSTGRESQL_VAR) \
		  src/static/ado-drivers-initialize.gpb $@

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

test-sqlite:		build-test regtests.db
ifeq ($(HAVE_SQLITE),yes)
	bin/ado_harness -p SQLite -t 120 -xml ado-sqlite-aunit.xml -config test-sqlite.properties
endif

test-mysql:		build-test create-mysql-tests
ifeq ($(HAVE_MYSQL),yes)
	bin/ado_harness -p MySQL -xml ado-mysql-aunit.xml -config test-mysql.properties
endif

test-postgresql:	build-test create-postgresql-tests
ifeq ($(HAVE_POSTGRESQL),yes)
	bin/ado_harness -p Postgresql -xml ado-postgresql-aunit.xml -config test-postgresql.properties
endif

CLEAN_FILES=src/static/ado-drivers-initialize.adb src/mysql/mysql-lib.ads regtests/ado-testsuite-drivers.adb

# Clean the root project of all build products.
clean::
	-rm -f $(CLEAN_FILES)

ifeq ($(HAVE_PANDOC),yes)
ifeq ($(HAVE_DYNAMO),yes)
doc::  docs/ado-book.pdf docs/ado-book.html
	$(DYNAMO) build-doc -markdown wiki

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
  Debugging.md

DOC_OPTIONS=-f markdown -o ado-book.pdf --listings --number-sections --toc
HTML_OPTIONS=-f markdown -o ado-book.html --listings --number-sections --toc --css pandoc.css

docs/ado-book.pdf: $(ADO_DOC_DEP) force
	$(DYNAMO) build-doc -pandoc docs
	cat docs/Model.md docs/ADO_Objects.md > docs/ADO_Model.md
	cd docs && pandoc $(DOC_OPTIONS) --template=./eisvogel.tex $(ADO_DOC)

docs/ado-book.html: docs/ado-book.pdf force
	cd docs && pandoc $(HTML_OPTIONS) $(ADO_DOC)

endif
endif

GENERATOR=dynamo

generate:
	$(GENERATOR) generate db/regtests
	$(GENERATOR) generate db/samples

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

install_dynamo:
	${MKDIR} -p ${dynamodir}/ado/db
	${CP} db/*.xml ${dynamodir}/ado/db/
ifeq ($(HAVE_MYSQL),yes)
	${MKDIR} -p ${dynamodir}/ado/db/mysql
	${CP} db/mysql/ado-*.sql ${dynamodir}/ado/db/mysql
endif
ifeq ($(HAVE_POSTGRESQL),yes)
	${MKDIR} -p ${dynamodir}/ado/db/postgresql
	${CP} db/postgresql/ado-*.sql ${dynamodir}/ado/db/postgresql
endif
ifeq ($(HAVE_SQLITE),yes)
	${MKDIR} -p ${dynamodir}/ado/db/sqlite
	${CP} db/sqlite/ado-*.sql ${dynamodir}/ado/db/sqlite
endif
	${CP} dynamo.xml ${dynamodir}/ado/
	${CP} NOTICE.txt ${dynamodir}/ado/
	${CP} LICENSE.txt ${dynamodir}/ado/

uninstall::
	rm -rf ${dynamodir}/ado

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
