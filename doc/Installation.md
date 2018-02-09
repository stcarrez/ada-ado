# Installation

This chapter explains how to build and install the library.

## Before Building

Before building ADO, you will need:

* [Ada Utility Library](https://github.com/stcarrez/ada-util)
* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/)
* Either the MySQL or SQLite development headers installed.

First get, build and install the [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/)
and then get, build and install the [Ada Utility Library](https://github.com/stcarrez/ada-util).

## Database Driver Installation

The MySQL and SQLite development headers and runtime are necessary for building
the ADO driver.  The configure script will use them to enable the ADO drivers.

### Ubuntu

MySQL Development installation
```
sudo apt-get install libmysqlclient-dev
```

SQLite Development installation
```
sudo apt-get install libsqlite3-dev
```

### Windows
For Windows, the installation is a little bit more complex and manual.
You may either download the files from MySQL and SQLite download sites
or you may use the files provided by ADO in the `win32` directory.

If your GNAT 2017 compiler is installed in `C:/GNAT/2017`, you may
install the MySQL and SQLite libraries by using msys cp with:

```
cp win32/*.dll C:/GNAT/2017/bin
cp win32/*.dll C:/GNAT/2017/lib
cp win32/*.lib C:/GNAT/2017/lib
cp win32/*.a C:/GNAT/2017/lib
```

## Configuration

The library uses the `configure` script to detect the build environment, check which databases
are available and configure everything before building.  If some component is missing, the
`configure` script will report an error.  The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--with-mysql=PATH` to control the path where `mysql_config` is installed,
  * `--with-ada-util=PATH` to control the installation path of [Ada Utility Library](https://github.com/stcarrez/ada-util),
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/ado_harness` test program.  A configuration
file is necessary to control the test parameters including the test database to be used.
To run the tests with a MySQL database, use the following command:
```
bin/ado_harness -config test-mysql.properties
```
and with a SQLite database, use the following command:
```
bin/ado_harness -config test-sqlite.properties
```

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```
