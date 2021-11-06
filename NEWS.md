Version 2.2.1   -
  - Fix #4: Is_Loaded predicate operation is false when an object is not yet inserted in the database
  - Fix #5: Exception raised when SQLite Query_Statement is finalized if the SQL query was invalid

Version 2.2.0   - Jul 2021
  - Improvement of query loaders
  - Fix reading database schema with float/double values
  - Rename Get_Double into Get_Long_Float

Version 2.1.2   - Feb 2021
  - Fix uninitialized float values in generated code used by unit tests

Version 2.1.1   - Nov 2020
  - Fix using the configuration with multiple config environments

Version 2.1.0   - May 2020
  - Added Is_Modified predicate on database objects
  - Fix SQLite Load_Schema to avoid loading SQLite specific tables

Version 2.0.0   - Dec 2019
  - Support for Postgresql database
  - Improvement for errors reported by database drivers
  - New audit framework to track database record changes
  - Added support for floating point numbers
  - Serialize queries in JSON/XML streams

Version 1.2.0   - Jul 2018
  - Improvement of SQLite connection management
  - Fix logs to avoid having password in clear text in logs
  - Fix lazy object loading

Version 1.1.0   - Dec 2015
  - Fix link issue on Fedora
  - Detect MariaDB as a replacement for MySQL
  - Improvement of configure and installation process with gprinstall (if available)

Version 1.0.1   - Jul 2014
  - Fix minor configuration issue with GNAT 2014

Version 1.0     - Apr 2014
  - Support to load query results in Ada bean datasets
  - Added support to load dynamic database drivers
  - Port on FreeBSD
  - Support for the creation of Debian packages

Version 0.4.0   - Feb 2013
  - Fix support to reload query definitions
  - Optimize session factory implementation
  - Customize the MySQL database connection by using MySQL SET

Version 0.3.0   - May 2012
  - Support to update database records when a field is really modified
  - Customize the SQLite database connection by using SQLite PRAGMAs
  - Escape MySQL or SQLite reserved keywords
  - Support for blob type

Version 0.2.0   - Aug 2011
  - Port on Windows

Version 0.1.0	- May 2011
  - ADO model with Mysql driver
  - ADO model with SQLite driver
