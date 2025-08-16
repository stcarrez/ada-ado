# Introduction

Ada Database Objects is an Ada 2012 library that provides database drivers
and object relational mapping to access a database in Ada.
The library supports Postgresql, MySQL/MariaDB, SQLite or SQLCipher as databases.
Altought not mandatory, it allows to map database objects into Ada records and access database content easily.
Most of the concepts developed for ADO come from the Java Hibernate ORM.

The ORM uses an YAML, XML mapping file or an UML model, a code generator and a runtime library
for the implementation.  It provides a database driver for [Postgresql](https://www.postgresql.org/),
[MySQL](https://www.mysql.com/)/[MariaDB](https://www.mariadb.org), [SQLite](https://www.sqlite.org/) or
[SQLCipher](https://www.zetetic.net/sqlcipher/).  The ORM helps your
application by providing a mapping of your database tables directly in the target programming
language: Ada05 in our case.  The development process is the following:

  * You design your database model either using a UML tool or by writing an XML or YAML description file,
  * You generate the Ada05 mapping files by using the [Dynamo](https://github.com/stcarrez/dynamo) code generator,
  * You generate the SQL database tables by using the same tool,
  * You write your application on top of the generated code that gives you direct and simplified access to your database.

![ORM Development Model](images/ado-orm.png)

This document describes how to build the library and how you can use
the different features to simplify and help you access databases
from your Ada application.

