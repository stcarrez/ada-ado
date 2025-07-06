Version 2.4.2   - Under development
  - Feature #26: Add support for SQLCipher
  - Feature #27: Simplify data source connection string for SQLite
  - Feature #29: Improve direct SQL query capabilities (Non-ORM)
  - Fix #25: Schema column 'text' not recognized by SQLite driver

Version 2.4.1   - Sep 2024
  - Cleanup build environment to drop configure

Version 2.4.0   - Aug 2023
  - Feature #11: Support to audit creation of new objects in the database
  - Feature #12: Support for database migration
  - Fix #14: Problems with SQLite transactions
  - Fix #15: Use ADO_BUILD instead of BUILD in the Alire crate configuration
  - Fix #16: Status SQLITE_ROW is treated as an error
  - Fix #18: Has_Table does not work for PostgreSQL

Version 2.3.0   - Aug 2022
  - Fix #4: Is_Loaded predicate operation is false when an object is not yet inserted in the database
  - Fix #5: Exception raised when SQLite Query_Statement is finalized if the SQL query was invalid
  - Fix #7: Update SQLite support to 3.31.1
  - Fix #8: Add SQlite busy handler to handle the SQLITE_BUSY error
  - Fix #9: Better handling of SQLITE_BUSY error
  - Fix #10: Error 'XML query file does not exist' when the query is loaded from a static embedded loader

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
