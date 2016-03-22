# Ada Database Objects

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-ADO.svg)](http://jenkins.vacs.fr/job/Ada-ADO/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-ADO.svg)](http://jenkins.vacs.fr/job/Ada-ADO/)
[![Download](https://img.shields.io/badge/download-1.0.1-brightgreen.svg)](http://download.vacs.fr/ada-ado/ada-ado-1.0.1.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-ado/v1.0.1.svg)

This Ada05 library provides object relational mapping
to access a database in Ada05.  Most of the concepts
developped for ADO come from the Java Hibernate ORM.

This library supports MySQL, SQLite as databases.
You need at least one of these databases (or both).

To build ADO, you will need:

* Ada Util (https://github.com/stcarrez/ada-util)
* XML/Ada  (http://libre.adacore.com/libre/tools/xmlada/)

You should have installed either MySQL or SQLite before
running the configure script.  For Windows, please read
the file 'win32/README' that gives some installation hints.

Build with the following commands:
```
   ./configure
   make
```
The unit tests are built and executed using:
```
   make test
```
And unit tests are executed with:
```
   bin/ado_harness -config test-mysql.properties
```
or
```
   bin/ado_harness -config test-sqlite.properties
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

   http://code.google.com/p/ada-ado/



# Database Drivers

The MySQL and SQLite development headers and runtime are necessary for building
the ADO driver.  The configure script will use them to enable the ADO drivers.

MySQL Development installation
```
      sudo apt-get install libmysqlclient-dev
```

SQLite Development installation
```
      sudo apt-get install libsqlite3-dev
```
For Windows, check win32/README to install the libraries.


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
To create manually the database, you can proceed to the following steps:

1. Create the 'ado_test' database in MySQL
```
         mysql -u root
         mysql> create database ado_test;
```
2. Create the 'ado' user and give the access rights:
```
         mysql> grant select, insert, update, delete,
                      create, drop, create temporary tables, execute,
                      show view on `ado_test`.* to ado@'localhost';
         mysql> flush privileges;
```
3. Create the tables
```
         mysql> use ado_test
         mysql> source db/regtests/mysql/create-ado-mysql.sql
```
