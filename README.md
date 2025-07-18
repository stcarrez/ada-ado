# Ada Database Objects

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/ado.json)](https://alire.ada.dev/crates/ado)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/ado_sqlite.json)](https://alire.ada.dev/crates/ado_sqlite)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/ado_mysql.json)](https://alire.ada.dev/crates/ado_mysql)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/ado_postgresql.json)](https://alire.ada.dev/crates/ado_postgresql)
[![Ada 2012](https://img.shields.io/badge/2012-inside-green?logo=ada&logoColor=white&logoSize=auto)](https://adaic.org/ada-resources/standards/ada12)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-ado/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-ado/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-ado/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-ado/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-ado/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-ado/summary)
[![Documentation Status](https://readthedocs.org/projects/ada-ado/badge/?version=latest)](https://ada-ado.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.4.1-brightgreen.svg)](http://download.vacs.fr/ada-ado/ada-ado-2.4.1.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-ado)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-ado/2.4.1.svg)](Commits)

Ada Database Objects is an Ada 2012 library that provides database drivers
and object relational mapping to access a database in Ada.
The library supports Postgresql, MySQL/MariaDB, SQLite or SQLCipher as databases.
Most of the concepts developped for ADO come from the Java Hibernate ORM.

The ORM uses an YAML, XML mapping file or an UML model, a code generator and a runtime library
for the implementation.  It provides a database driver for [Postgresql](https://www.postgresql.org/),
[MySQL](https://www.mysql.com/)/[MariaDB](https://www.mariadb.org), [SQLite](https://www.sqlite.org/) or
[SQLCipher](https://www.zetetic.net/sqlcipher/).  The ORM helps your
application by providing a mapping of your database tables directly in the target programming
language: Ada 2012 or Ada 2022 in our case.  The development process is the following:

  * You design your database model either using a UML tool or by writing a YAML or XML description,
  * You generate the Ada mapping files by using the [Dynamo](https://gitlab.com/stcarrez/dynamo) code generator,
  * You generate the SQL database tables by using the same tool,
  * You write your application on top of the generated code that gives you direct and simplified access to your database.

![ADO Development model](https://github.com/stcarrez/ada-ado/wiki/images/ado-orm.png)

If you don't want to use the ORM layer, you can still use Ada Database Objects and perform traditional
SQL queries.  You need at least one of these databases (or all of then),
check the [Database Drivers](#database-drivers)
section to install them.

## Version 2.4.2   - Under development
  - Feature #26: Add support for SQLCipher
  - Feature #27: Simplify data source connection string for SQLite
  - Feature #29: Improve direct SQL query capabilities (Non-ORM)
  - Fix #25: Schema column 'text' not recognized by SQLite driver

## Version 2.4.1   - Sep 2024
  - Cleanup build environment to drop configure

[List all versions](https://gitlab.com/stcarrez/ada-ado/blob/master/NEWS.md)

## Using with Alire

If you are using [Alire](https://alire.ada.dev/) in your project, run the following command
within your [Alire](https://alire.ada.dev/) project to use the library:

```
alr with ado
```

Depending on your project, you may need one or some of the following other components
to get the support for SQLite, MySQL/MariaDB or PostgreSQL. Use ado_mysql for MariaDB.

```
alr with ado_sqlite
alr with ado_mysql
alr with ado_postgresql
```

## Using without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory.
For a detailed description on how you can configure, build and install the library
refer to the [Installation](https://ada-ado.readthedocs.io/en/latest/Installation/) guide.
Otherwise, you can easily configure and build the library with the steps described below.

The support for SQLite, SQLCipher, MySQL/MariaDB and PostgreSQL are enabled only when a `HAVE_XXX=yes` configuration
variable is defined.  Run the setup command that records in the `Makefile.conf` the configuration
you want to build.

The `HAVE_ALIRE` configuration allows you to build with [Alire](https://alire.ada.dev/) or not.

The example below enables the SQLite and PostgreSQL components but disables
the MySQL/MariaDB support and disables the use of [Alire](https://alire.ada.dev/) to build
the library.

```
make setup BUILD=debug PREFIX=/build/install \
  HAVE_SQLITE=yes HAVE_SQLCIPHER=yes HAVE_POSTGRESQL=yes \
  HAVE_MYSQL=no HAVE_ALIRE=no
```

Then build, run the unit tests and install by using:

```
make
make test
make install
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

# Samples

The samples can be built using:

```
cd samples
alr build
```

If you want to build the examples with SQLcipher, use:

```
cd samples
alr build -- -XADO_USE_SQLCIPHER=yes
```

Before launching the samples, the database must have been created.
For SQLite, use:

```
make samples.db
```

For SQLCipher, use:

```
make samples.cipher
```

For PostgreSQL and MySQL, create the database and setup the schema by using either `db/samples/postgresql/create-ado-postgresql.sql` or
`db/samples/mysql/create-ado-mysql.sql`.

Before launching the example, look at the `samples.properties` or `samples/samples.properties` file and comment or uncomment
one of the following lines to choose the database (below, the configuration using SQLite):

```
# ado.database=mysql://localhost:3306/ado_test?user=ado&encoding=utf8
# ado.database=postgresql://localhost:5432/ado_test?user=ado&password=ado
# ado.database=sqlite:samples.cipher?key='samples-password'&synchronous=OFF&encoding='UTF-8'&journal_mode=WAL
ado.database=sqlite:samples.db?synchronous=OFF&encoding='UTF-8'&journal_mode=WAL
```


## Examples overview

| Source              | Example                                                                      |
|----------------------------------|------------------------------------------------------------------------------|
| [sql_list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_list_user.adb) | List users (`SELECT...FROM`) |
| [sql_add_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_add_user.adb) | Insert user (`INSERT INTO...`) |
| [sql_del_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_del_user.adb) | Delete user (`DELETE FROM...`) |
| [list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/list_user.adb) | ORM version of list users (`SELECT...FROM`) |
| [add_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/add_user.adb) | ORM version of insert users (`INSERT INTO`) |
| [del_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/del_user.adb) | ORM version of delete users (`DELETE FROM`) |
| [print_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/print_user.adb) | ORM version of find user (`SELECT...`) |
| [pschema.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/pschema.adb) | Print database schema |

### SQL only examples

| Package              | Example                                                                      |
|----------------------------------|------------------------------------------------------------------------------|
| `ADO.Sessions` | [sql_list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_list_user.adb)|
| `ADO.Statements` | [sql_list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_list_user.adb) [sql_add_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_add_user.adb) [sql_del_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/sql_del_user.adb)|
| `ADO.Schemas` | [pschema.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/pschema.adb)|
| `ADO.Sessions.Load_Schema` | [pschema.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/pschema.adb)|

### ORM examples

| Package              | Example                                                                      |
|----------------------------------|------------------------------------------------------------------------------|
| `ADO.Sessions`  | [list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/list_user.adb)|
| `ADO.Queries`   | [list_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/list_user.adb) [userdb.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/userdb.adb)|
| `ADO.Objects`   | [userdb.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/userdb.adb)|
| `ADO.SQL`       | [print_user.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/print_user.adb) [userdb.adb](https://github.com/stcarrez/ada-ado/tree/master/samples/src/userdb.adb)|


# Documentation

The Ada Database Objects sources as well as a wiki documentation is provided on:

  * [Ada Database Objects Programmer's Guide](https://ada-ado.readthedocs.io/en/latest/) [PDF](https://gitlab.com/stcarrez/ada-ado/blob/master/docs/ado-book.pdf)
  * [ADO Documentation](https://gitlab.com/stcarrez/ada-ado/wiki)
  * [ADO Sessions](https://gitlab.com/stcarrez/ada-ado/wiki/ADO_Sessions)
  * [ADO Statements](https://gitlab.com/stcarrez/ada-ado/wiki/ADO_Statements)
  * [ADO Queries](https://gitlab.com/stcarrez/ada-ado/wiki/ADO_Queries)
  * [ADO Drivers](https://gitlab.com/stcarrez/ada-ado/wiki/ADO_Drivers)

# Presentations

  * [Persistence with Ada Database Objects](https://fr.slideshare.net/StephaneCarrez1/persistence-with-ada-database-objects-ado) FOSDEM 2019
  * Uses Ada Database Objects: [Implementing a build manager in Ada](https://www.slideshare.net/StephaneCarrez1/implementing-a-build-manager-in-ada)

# Database Drivers

The Postgresql, MySQL/MariaDB and SQLite development headers and runtime are necessary for building
the ADO driver.  The configure script will use them to enable the ADO drivers.

Postgresql development installation
```
sudo apt-get install postgresql-client libpq-dev
```

MySQL/MariaDB Development installation
```
sudo apt-get install mariadb-client libmariadb-dev
```

SQLite development installation
```
sudo apt-get install sqlite3 libsqlite3-dev
```

SQLCipher development installation
```
sudo apt-get install sqlcipher libsqlcipher-dev
```

For Windows, check [win32/README](win32/README.md) to install the libraries.


## Database Creation

Create the tests database by using the Dynamo command.
(Dynamo is available at: https://gitlab.com/stcarrez/dynamo)
Note: change 'root' and 'password' to a MySQL/MariaDB user that has admin access rights
('create database' and 'grant option' privileges).
```
dynamo create-database db/regtests root password
```
The default database connection string is defined in dynamo.xml.
You can also specify the connection string and create the schema by using:
```
dynamo create-database db/regtests 'mysql://localhost:3306/ado_test?user=ado' root password
```

### MySQL/MariaDB setup

To create manually the database, you can proceed to the following steps:

1. Create the 'ado_test' database in MySQL/MariaDB
```
sudo mysql
mysql> CREATE DATABASE ado_test;
```

2. Create the 'ado' user and give the access rights:
```
mysql> CREATE USER 'ado' IDENTIFIED BY '';
```

```
mysql> GRANT SELECT, INSERT, UPDATE, DELETE,
       CREATE, DROP, CREATE TEMPORARY TABLES, EXECUTE,
       SHOW VIEW ON `ado_test`.* TO ado@'%';
mysql> FLUSH PRIVILEGES;
```

3. Create the tables
```
mysql> USE ado_test
mysql> SOURCE db/regtests/mysql/create-ado-mysql.sql
```

### Postgresql setup

To create manually the database, you can proceed to the following steps:

1. Create the 'ado' user and configure the password
(enter 'ado' for the password or update the Makefile as well as the test-postgresql.properties file):
```
sudo -u postgres createuser ado --pwprompt
```

2. Create the 'ado_test' database in Postgresql
```
sudo -u postgres createdb -O ado ado_test
```

3. Create the tables
```
psql "postgresql://localhost:5432/ado_test?user=ado&password=ado" \
  --file=db/regtests/postgresql/create-ado-postgresql.sql
```

# Testing

Before running the unit tests for MySQL/MariaDB or Postgresql, you must create the
test database as described in [Database Creation](#database-creation).

The unit tests are built and executed using:
```
make test
```
To run manually the unit tests, use the following commands:
```
bin/ado_harness -config test-mysql.properties
```
or
```
bin/ado_harness -config test-sqlite.properties
```
or
```
bin/ado_harness -config test-sqlcipher.properties
```
or
```
bin/ado_harness -config test-postgresql.properties
```

# Building documentation

The *ADO Programmer's Guide* is generated by using [Dynamo](https://gitlab.com/stcarrez/dynamo)
and [Pandoc](https://pandoc.org).  You may need the following Debian packages:

```
sudo apt-get install pandoc xsltproc texlive-latex-base texlive-latex-extra texlive-fonts-extra
```

The documentation is created by the following command:
```
make doc
```

and the book is generated in the *ado-book.pdf* file.

