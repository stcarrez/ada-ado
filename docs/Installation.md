# Installation

This chapter explains how to build and install the library.

## Using Alire

The Ada Database Objects library is available as several Alire crates to simplify the installation
and setup your project.  Run the following commands to setup your project to use the library:

```
alr index --update-all
alr with ado
```

The database drivers for SQLite/SQLCipher, MySQL/MariaDB and PostgreSQL are provided as separate crates. Choose the 
database drivers you want to use and run one or some of the commands below. For MariaDB use ado_mysql. 

```
alr with ado_sqlite
alr with ado_mysql
alr with ado_postgresql
```

## Without Alire

### Database Driver Installation

The PostgreSQL, MySQL/MariaDB and SQLite/SQLCipher development headers and runtime are necessary for building
the ADO driver.  The configure script will use them to enable the ADO drivers.
The configure script will fail if it does not find any database driver.

#### Ubuntu

MySQL/MariaDB development installation
```
sudo apt-get install libmysqlclient-dev
```

MariaDB development installation
```
sudo apt-get install mariadb-client libmariadb-client-lgpl-dev
```

SQLite development installation
```
sudo apt-get install libsqlite3-dev
```

SQLCipher development installation
```
sudo apt-get install sqlcipher libsqlcipher-dev
```

PostgreSQL development installation
```
sudo apt-get install postgresql-client libpq-dev
```

#### Windows

It is recommended to use msys2 available at https://www.msys2.org/
and use the `pacman` command to install the required packages.

```
pacman -S git
pacman -S make
pacman -S unzip
pacman -S base-devel --needed
pacman -S mingw-w64-x86_64-sqlite3
```

For Windows, the installation is a little bit more complex and manual.
You may either download the files from MySQL/MariaDB and SQLite download 
sites or you may use the files provided by Ada Database Objects
in the `win32` directory.

For Windows 32-bit, extract the files:

```
cd win32 && unzip sqlite-dll-win32-x86-3290000.zip
```

For Windows 64-bit, extract the files:

```
cd win32 && unzip sqlite-dll-win64-x64-3290000.zip
```

If your GNAT 2019 compiler is installed in `C:/GNAT/2019`, you may
install the liblzma, MySQL/MariaDB and SQLite libraries by using msys 
cp with:

```
cp win32/*.dll C:/GNAT/2019/bin
cp win32/*.dll C:/GNAT/2019/lib
cp win32/*.lib C:/GNAT/2019/lib
cp win32/*.a C:/GNAT/2019/lib
```


### Before Building

Before building ADO, you will need:

* [Ada Utility Library](https://github.com/stcarrez/ada-util)
* [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/)
* Either the PostgreSQL, MySQL/MariaDB or SQLite development headers installed.

First get, build and install the [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/)
and then get, build and install the [Ada Utility Library](https://github.com/stcarrez/ada-util).

### Configuration

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

Note: if you plan to use both SQLite and SQLCipher databases, use the `HAVE_SQLCIPHER=yes` configuration since
SQLCipher supports non-encrypted databases.

### Build

After configuration is successful, you can build the library by running:

```
make
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:

```
make test
```

And unit tests are executed by running the `bin/ado_harness` test program.  A configuration
file is necessary to control the test parameters including the test database to be used.
To run the tests with a MySQL/MariaDB database, use the following command:
```
bin/ado_harness -config test-mysql.properties
```
and with a SQLite database, use the following command:
```
bin/ado_harness -config test-sqlite.properties
```

### Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```

## Using

To use the library in an Ada project, add the following line at the beginning of your
GNAT project file:

```
with "ado";
with "ado_all";
```

It is possible to use only a specific database driver, in that case your GNAT
project file could be defined as follows:

```
with "ado";
with "ado_mysql";
with "ado_sqlite";
with "ado_postgresql";
```

where the `ado_mysql`, `ado_sqlite` and `ado_postgresql` are optional and
included according to your needs.
