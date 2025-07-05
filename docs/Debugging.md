# Troubleshooting

## Change the log configuration

The ADO runtime uses the logging framework provided by Ada Utility Library.
By default, logging messages are disabled and the logging framework has a
negligeable impact on performance (less than 1 us per log).

You can customize the logging framework so that you activate logs
according to your needs.  In the full mode, the ADO runtime will report
the SQL statements which are executed.

To control the logging, add or update the following definitions in a
property file:
```
log4j.rootCategory=DEBUG,console,result

log4j.appender.console=Console
log4j.appender.console.level=WARN
log4j.appender.console.layout=level-message

log4j.appender.result=File
log4j.appender.result.File=test.log

# Logger configuration
log4j.logger.ADO=INFO,result
log4j.logger.ADO.Sessions=WARN
log4j.logger.ADO.Statements=DEBUG
```

The logging framework is configured by using the `Util.Log.Logging.Initialize` operation:

```
   Util.Log.Loggers.Initialize ("config.properties");
```
which can be executed from any place (but the best place is during the application start).

You can also configure the logger in Ada by using the following code:

```
with Util.Properties;
...
   Log_Config  : Util.Properties.Manager;
   ...
   Log_Config.Set ("log4j.rootCategory", "DEBUG,console");
   Log_Config.Set ("log4j.appender.console", "Console");
   Log_Config.Set ("log4j.appender.console.level", "ERROR");
   Log_Config.Set ("log4j.appender.console.layout", "level-message");
   Log_Config.Set ("log4j.logger.Util", "FATAL");
   Log_Config.Set ("log4j.logger.ADO", "ERROR");
   Log_Config.Set ("log4j.logger.ADO.Statements", "DEBUG");
   Util.Log.Loggers.Initialize (Log_Config);                                    
```

The ADO runtime has several loggers, each of them can be activated separately.
The following loggers are interesting:

| Logger name    | Description                                |
| -------------- | ------------------------------------------ |
| ADO.Drivers    | Database drivers and connection to servers |
| ADO.Sessions   | Database session management                |
| ADO.Statements | SQL statements execution                   |
| ADO.Queries    | Named queries identification and retrieval |

## Handling exceptions

Some exceptions are raised when there is a serious problem.
The problem could be of different nature:

  * there is a database connection issue,
  * there is an SQL error,
  * there is a data inconsistency.

The `ADO.Sessions.Connection_Error` exception is raised when the connection string used
to access the database is incorrect.  The connection string could be improperly formatted,
a database driver may not be found, the database server may not be reachable.

The `ADO.Sessions.Session_Error` exception is raised when the `Session` object is used
while it is not initialized or the connection was closed programatically.

The `ADO.Queries.Query_Error` exception is raised when a named query cannot be found.
In that case, the SQL that corresponds to the query cannot be executed.

The `ADO.Statements.SQL_Error` exception is raised when the execution of an SQL query
fails. This is an indication that the SQL statement is invalid and was rejected by
the database.

The `ADO.Statements.Invalid_Column` exception is raised after the execution of an SQL
query when the application tries to access the result.  It is raised when the program
tries to retrieve a column value that does not exist.

The `ADO.Statements.Invalid_Type` exception is also raised after the execution of
an SQL query when the value of a column cannot be converted to the Ada type.
It occurs if a column contains a string while the application tries to get the
column as an integer or date.  Similarly, if a column is null and the returned
Ada type does not support the nullable concept, this exception will be raised.

The `ADO.Statements.Invalid_Statement` exception is raised when you try to use and
execute a `Statement` object which is not initialized.

The object layer provided by ADO raises specific exceptions.

The `ADO.Objects.NOT_FOUND` exception is raised by the generated `Load` procedure
when an object cannot be found in the database.

The `ADO.Objects.INSERT_ERROR` exception is raised by the generated `Save` procedure
executed the SQL INSERT statement and its execution failed.

The `ADO.Objects.UPDATE_ERROR` exception is raised by the generated `Save` procedure
executed the SQL UPDATE statement and its execution failed.

The `ADO.Objects.LAZY_LOCK` exception is raised by the generated `Save` procedure
executed the SQL UPDATE statement failed and the version of the object was changed.


