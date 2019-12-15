# Ada Database Objects

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-ADO.svg)](https://jenkins.vacs.fr/job/Ada-ADO/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-ADO.svg)](https://jenkins.vacs.fr/job/Ada-ADO/)
[![Documentation Status](https://readthedocs.org/projects/ada-ado/badge/?version=latest)](https://ada-ado.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.0.0-brightgreen.svg)](http://download.vacs.fr/ada-ado/ada-ado-2.0.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-ado/2.0.0.svg)

Ada Database Objects is an Ada05 library that provides
object relational mapping to access a database in Ada05.
The library supports Postgresql, MySQL, SQLite as databases.
Most of the concepts developped for ADO come from the Java Hibernate ORM.

The ORM uses an YAML, XML mapping file or an UML model, a code generator and a runtime library
for the implementation.  It provides a database driver for [Postgresql](https://www.postgresql.org/),
[MySQL](https://www.mysql.com/) and [SQLite](https://www.sqlite.org/).  The ORM helps your
application by providing a mapping of your database tables directly in the target programming
language: Ada05 in our case.  The development process is the following:

  * You design your database model either using a UML tool or by writing a YAML or XML description,
  * You generate the Ada05 mapping files by using the [Dynamo](https://github.com/stcarrez/dynamo) code generator,
  * You generate the SQL database tables by using the same tool,
  * You write your application on top of the generated code that gives you direct and simplified access to your database.

![ADO Development model](https://github.com/stcarrez/ada-ado/wiki/images/ado-orm.png)

You need at least one of these databases (or all of then).  The configure script will now
fail if no supported database was found.  Check the [Database Drivers](#database-drivers)
section to install them and run the configure again after the installation.

# Build

To build ADO, you will need:

* [Ada Util](https://github.com/stcarrez/ada-util)
* [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/)

You should have installed either Postgresql, MySQL or SQLite before
running the configure script.  For Windows, please read
the file [win32/README](win32/README.md) that gives some installation hints.

Build with the following commands:
```
./configure
make
```

# Samples

The samples can be built using:
```
gnatmake -Psamples
```   

Before launching the samples, the database must have been created.
For SQLite, use:
```
make samples.db
```

# Documentation

The Ada Database Objects sources as well as a wiki documentation is provided on:

  * [Ada Database Objects Programmer's Guide](https://ada-ado.readthedocs.io/en/latest/)
  * [ADO Documentation](https://github.com/stcarrez/ada-ado/wiki)
  * [ADO Sessions](https://github.com/stcarrez/ada-ado/wiki/ADO_Sessions)
  * [ADO Statements](https://github.com/stcarrez/ada-ado/wiki/ADO_Statements)
  * [ADO Queries](https://github.com/stcarrez/ada-ado/wiki/ADO_Queries)
  * [ADO Drivers](https://github.com/stcarrez/ada-ado/wiki/ADO_Drivers)

# Presentations

  * [Persistence with Ada Database Objects](https://fr.slideshare.net/StephaneCarrez1/persistence-with-ada-database-objects-ado) FOSDEM 2019

# Database Drivers

The Postgresql, MySQL and SQLite development headers and runtime are necessary for building
the ADO driver.  The configure script will use them to enable the ADO drivers.

Postgresql Development installation
```
sudo apt-get install postgresql-client libpq-dev
```

MySQL Development installation
```
sudo apt-get install mariadb-client libmariadb-client-lgpl-dev
```

SQLite Development installation
```
sudo apt-get install sqlite3 libsqlite3-dev
```
For Windows, check [win32/README](win32/README.md) to install the libraries.


## Database Creation

Create the tests database by using the Dynamo command.
(Dynamo is available at: https://github.com/stcarrez/dynamo)
Note: change 'root' and 'password' to a MySQL user that has admin access rights
('create database' and 'grant option' privileges).
```
dynamo create-database db/regtests root password
```
The default database connection string is defined in dynamo.xml.
You can also specify the connection string and create the schema by using:
```
dynamo create-database db/regtests 'mysql://localhost:3306/ado_test?user=ado' root password
```

### MySQL setup

To create manually the database, you can proceed to the following steps:

1. Create the 'ado_test' database in MySQL
```
mysql -u root
mysql> CREATE DATABASE ado_test;
```

2. Create the 'ado' user and give the access rights:
```
mysql> CREATE USER 'ado_test' IDENTIFIED BY '';
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

Before running the unit tests for MySQL or Postgresql, you must create the
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
bin/ado_harness -config test-postgresql.properties
```

# Building documentation

The *ADO Programmer's Guide* is generated by using [Dynamo](https://github.com/stcarrez/dynamo)
and [Pandoc](https://pandoc.org).  You may need the following Debian packages:

```
sudo apt-get install pandoc xsltproc texlive-latex-base texlive-latex-extra texlive-fonts-extra
```

The documentation is created by the following command:
```
make doc
```

and the book is generated in the *ado-book.pdf* file.

