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


