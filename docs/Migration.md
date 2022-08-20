# Database schema migration

In the lifetime of an application, the database schema may be changed and these changes
must be applied to the database when a new version of the application is installed.
Each application can implement its own mechanism to track the database schema change
and apply the necessary change to the database when the application is upgrated.

An automatic tracking and update is too difficult for this library because
such database migration is specific to each application.  However, the 
Ada Database Objects library provides a mechanism to help in updating the database
schema.  The proposed mechanism is based on:

* the `ado_version` database table which tracks for each application module, a schema
  version.  The schema version is changed by the developer when a new database schema
  is made for the module and some SQL migration script must be executed.  The version
  is a simple integer that is incremented when the schema is modified.
* a set of SQL migration scripts that control the migration of a module from a version
  to the next one.  The SQL migration scripts are written manually by the developer
  (or by some generation tool if they exist) when the database schema is modified.

The SQL migration scripts must be organized on the file system so that the library
can find them and decide whether they must be executed or not.  Each SQL migration script
of a module must be stored in a directory with the name of that module.  Then, each
upgrade for a schema version of the module must also be stored in a directory with
the name of the version.  To control the order of execution of SQL migration scripts,
each script file must be prefixed by a unique order.  SQL scripts are executed in
increasing order.

The layout is the following:

```
db/migrate/<module>/<version>/depend.conf
db/migrate/<module>/<version>/<order>-all-<name>.sql
db/migrate/<module>/<version>/<order>-<driver>-<name>.sql
```

To proceed to the migration, the library will do the following:

* it loads from the database the `ado_version` records which indicates for each
  module the current schema version used,
* it scans the `db/migrate` directory which contains the module SQL migration
  scripts.  For each database module, it identifies the module versions that
  must be upgraded.  A module needs an upgrade when there is a `<version>`
  directory that exists and is higher than the current schema version recorded
  in `ado_version`.
* for a given module, it checks the dependencies and make sure that the 
  dependent schemas are updated to the specified dependent version,

