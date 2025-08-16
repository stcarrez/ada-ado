# Session
The `ADO.Sessions` package defines the control and management of database sessions.
The database session is represented by the `Session` or `Master_Session` types.
It provides operation to create a database statement that can be executed.
The `Session` type is used to represent read-only database sessions.  It provides
operations to query the database but it does not allow to update or delete content.
The `Master_Session` type extends the `Session` type to provide write
access and it provides operations to get update or delete statements.  The differentiation
between the two sessions is provided for the support of database replications with
databases such as MySQL.

## Connection string
The database connection string is an URI that specifies the database driver to use as well
as the information for the database driver to connect to the database.
The driver connection is a string of the form:

```Ada
driver://[host][:port]/[database][?property1][=value1]...
```

The database connection string is passed to the session factory that maintains connections
to the database (see ADO.Sessions.Factory).

## Database Drivers
Database drivers provide operations to access the database.  These operations are
specific to the database type and the `ADO.Drivers` package among others provide
an abstraction that allows to make the different databases look like they have almost
the same interface.

A database driver exists for SQLite/SQLCipher, MySQL and PostgreSQL. The driver
is either statically linked to the application or it can be loaded dynamically if it was
built as a shared library.  For a dynamic load, the driver shared library name must be
prefixed by `libada_ado_`.  For example, for a `mysql` driver, the shared
library name is `libada_ado_mysql.so`.

| Driver name | Database          |
| ----------- | ---------         |
| mysql       | MySQL, MariaDB    |
| sqlite      | SQLite, SQLCipher |
| postgresql  | PostgreSQL        |

The database drivers are initialized automatically but in some cases, you may want
to control some database driver configuration parameter.  In that case,
the initialization must be done only once before creating a session
factory and getting a database connection.  The initialization can be made using
a property file which contains the configuration for the database drivers and
the database connection properties.  For such initialization, you will have to
call one of the `Initialize` operation from the `ADO.Drivers` package.

```Ada
ADO.Drivers.Initialize ("db.properties");
```

The set of configuration properties can be set programatically and passed to the
`Initialize` operation.

```Ada
Config : Util.Properties.Manager;
...
  Config.Set ("ado.database", "sqlite:///mydatabase.db");
  Config.Set ("ado.queries.path", ".;db");
  ADO.Drivers.Initialize (Config);
```

Once initialized, a configuration property can be retrieved by using the `Get_Config`
operation.

```Ada
URI : constant String := ADO.Drivers.Get_Config ("ado.database");
```

Dynamic loading of database drivers is disabled by default for security reasons and
it can be enabled by setting the following property in the configuration file:

```Ada
ado.drivers.load=true
```

Dynamic loading is triggered when a database connection string refers to a database
driver which is not known.

### MySQL Database Driver
The MySQL database driver can be initialize explicitly by using the `ado_mysql`
GNAT project and calling the initialization procedure.

```Ada
ADO.Mysql.Initialize ("db.properties");
```

The set of configuration properties can be set programatically and passed to the
`Initialize` operation.

```Ada
Config : Util.Properties.Manager;
...
  Config.Set ("ado.database", "mysql://localhost:3306/ado_test");
  Config.Set ("ado.queries.path", ".;db");
  ADO.Mysql.Initialize (Config);
```

The MySQL database driver supports the following properties:

| Name        | Description       |
| ----------- | ---------      |
| user        | The user name to connect to the server |
| password    | The user password to connect to the server |
| socket      | The optional Unix socket path for a Unix socket base connection |
| encoding    | The encoding to be used for the connection (ex: UTF-8) |

### SQLite Database Driver
The SQLite database driver can be initialized explicitly by using the `ado_sqlite`
GNAT project and calling the initialization procedure.

```Ada
ADO.Sqlite.Initialize ("db.properties");
```

The set of configuration properties can be set programatically and passed to the
`Initialize` operation.

```Ada
Config : Util.Properties.Manager;
...
  Config.Set ("ado.database", "sqlite:regtests.db?synchronous=OFF&encoding=UTF-8");
  Config.Set ("ado.queries.path", ".;db");
  ADO.Sqlite.Initialize (Config);
```

The SQLite database driver will pass all the properties as SQLite `pragma` allowing
the configuration of the SQLite database.  When the driver is compiled with the
`-XADO_USE_SQLCIPHER=yes` option, the SQLCipher runtime will be used and you can
either use a standard SQLite database or an SQLCipher database.  The database
encryption key must be configured as a `key` configuration property, for
example:

```Ada
  Config.Set ("ado.database",
              "sqlite:regtests.cipher?key='db-password'");
```

### PostgreSQL Database Driver
The PostgreSQL database driver can be initialize explicitly by using the `ado_postgresql`
GNAT project and calling the initialization procedure.

```Ada
ADO.Postgresql.Initialize ("db.properties");
```

The set of configuration properties can be set programatically and passed to the
`Initialize` operation.

```Ada
Config : Util.Properties.Manager;
...
  Config.Set ("ado.database", "postgresql://localhost:5432/ado_test?user=ado&password=ado");
  Config.Set ("ado.queries.path", ".;db");
  ADO.Postgresql.Initialize (Config);
```

The PostgreSQL database driver supports the following properties:

| Name        | Description       |
| ----------- | ---------      |
| user        | The user name to connect to the server |
| password    | The user password to connect to the server |

## Session Factory
The session factory is the entry point to obtain a database session.
The `ADO.Sessions.Factory` package defines the factory for creating
sessions.

```Ada
with ADO.Sessions.Factory;
...
Sess_Factory : ADO.Sessions.Factory;
```

The session factory can be initialized by using the `Create` operation and
by giving a URI string that identifies the driver and the information to connect
to the database.  The session factory is created only once when the application starts.

```Ada
ADO.Sessions.Factory.Create (Sess_Factory, "mysql://localhost:3306/ado_test?user=test");
```

Having a session factory, one can get a database by using the `Get_Session` or
`Get_Master_Session` function.  Each time this operation is called, a new session
is returned.  The session is released when the session variable is finalized.

```Ada
DB : ADO.Sessions.Session := Sess_Factory.Get_Session;
```

The session factory is also responsible for maintaining some data that is shared by
all the database connections.  This includes:

  * the sequence generators used to allocate unique identifiers for database tables,
  * the entity cache,
  * some application specific global cache.

## Database Caches
The ADO cache manager allows to create and maintain cache of values and use the cache
from the SQL expander to replace cached values before evaluating the SQL.  The SQL expander
identifies constructs as follows:

```Ada
$cache_name[entry-name]
```

and look for the cache identified by <tt>cache_name</tt> and then replace the cache entry
registered with the name <tt>entry-name</tt>.

The cache manager is represented by the <tt>Cache_Manager</tt> type and the database
session contains one cache manager.  Applications may use their own cache in that case
they will declare their cache as follows:

```Ada
 M : ADO.Caches.Cache_Manager;
```

A cache group is identified by a unique name and is represented by the <tt>Cache_Type</tt>
base class.  The cache group instance is registered in the cache manager by using the
<tt>Add_Cache</tt> operation.

