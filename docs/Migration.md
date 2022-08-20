# Database schema migration

In the lifetime of an application, the database schema may be changed and these changes
must be applied to the database when a new version of the application is installed.
Each application can implement its own mechanism to track the database schema change
and apply the necessary change to the database when the application is upgraded.

## Migration mechanism

An automatic tracking and update is too difficult for this library because
such database migration is specific to each application.  However, the
Ada Database Objects library provides a mechanism to help in updating the database
schema.  The proposed mechanism is based on:

* the `ado_version` database table which tracks for each application module, a schema
  version.  The schema version is changed by the developer when a new database schema
  is made for the module and some SQL migration script must be executed.  The version
  is a simple integer that is incremented by the developer when the schema is modified.
* a set of SQL migration scripts that control the migration of a module from a version
  to the next one.  The SQL migration scripts are written manually by the developer
  (or by some generation tool if they exist) when the database schema is modified.

The SQL migration scripts must be organized on the file system so that the library
can find them and decide whether they must be executed or not.  Each SQL migration script
of a module must be stored in a directory with the name of that module.  Then, each
upgrade for a schema version of the module must also be stored in a directory with
the name of the version.  To control the order of execution of SQL migration scripts,
each script file must be prefixed by a unique order.  SQL scripts are executed in
increasing order.  Following the unique order, a tag separated by `-` indicates
the database driver that corresponds to the SQL script.  The special tag `all`
indicates that the script must be executed for all database drivers.  This allows
to have specific SQL for different database types such as SQLite, PostgreSQL or MySQL.

To summarize, the layout is the following:

```
db/migrate/<module>/<version>/depend.conf
db/migrate/<module>/<version>/<order>-all-<name>.sql
db/migrate/<module>/<version>/<order>-<driver>-<name>.sql
```

The database migration support is decomposed in several steps
each represented by a separate procedure so that it provides a fine
grain control over the migration execution.  For example it is possible to
print the list of SQL scripts without executing them.  It is also possible
to perform partial migration and upgrade only a subset of the list by
stopping at any time.

To proceed with the migration, the library will do the following:

* it scans the `db/migrate` directory which contains the module SQL migration
  scripts.  For each database module, it identifies the module versions that
  must be upgraded.  A module needs an upgrade when there is a `<version>`
  directory that exists and is higher than the current schema version recorded
  in `ado_version`.  This step is done by the `ADO.Schemas.Databases.Scan_Migration`
  procedure.  Upon completion of this step, we get a list of `Update_Type`
  records that indicate every upgrade that must be executed.
* the list of upgrades must then be sorted to honor the dependencies between
  the database modules.  This step must be done to make sure that the 
  dependent schemas are updated to the specified dependent version.
  This step is executed by the `ADO.Schemas.Databases.Sort_Migration` procedure.
* having a sorted list of upgrade, it is now possible to scan the directory
  that contains the SQL scripts (ie `db/migrate/<module>/<version>`).
  When reading that directory, we only look at the SQL files with the `.sql`
  extension.  The file name must follow the pattern `<order>-<tag>-<title>.sql`
  (or the regular expression `[0-9]+-(all|mysql|postgresql|sqlite)-.*\.sql`).
  The SQL file is taken into account if it uses the tag `all` or uses the database
  driver name of the database being upgraded.  The final list of SQL files is
  then sorted.  This step is handled by the `ADO.Schemas.Prepare_Migration`
  procedure.  It produces a list of files with their absolute path in the
  order in which they must be executed for the module upgrade for the specific
  version.
* the last step is to execute the SQL statements from the list of files
  computed previously.  During this step, the SQL file is read and split into
  SQL statements that are executed on the database.  Once every statement is
  executed, the next file is processed until the last one.  At the end, the
  version associated with the database module is update to the upgraded version.
  If the `ado_version` table does not contain the module, an entry is created
  with the version.  This step is made by the `ADO.Schemas.Run_Migration`
  procedure.  The `Run_Migration` procedure will also automatically call
  `Prepare_Migration` to collect the list of files if necessary.

The following code extract illustrates a complete database migration
support:

```
with ADO.Sessions;
with ADO.Schemas.Databases;
...
Session : ADO.Sessions.Master_Session;
List    : ADO.Schemas.Databases.Upgrade_List;
Files   : Util.Strings.Vectors.Vector;
...
   ADO.Schemas.Databases.Scan_Migration (Session, "db/migrate", List);
   ADO.Schemas.Databases.Sort_Migration (List);
   for Upgrade of List loop
      ADO.Schemas.Databases.Run_Migration
        (Session, Upgrade, Files, ADO.Sessions.Execute'Access);
   end loop;
```

## Taking into account the migration during development

This section gives some tips to take into account database schema
changes during development.

When there is a database schema change in a module, the version should
be incremented and one or several SQL scripts to upgrade only to the new
version are written.  The schema change must be identified and analyzed either
manually or with some tool.  For example if a new column is added to some
table, you will have to write some SQL that looks like:

```
ALTER TABLE awa_post ADD COLUMN `read_count` INTEGER NOT NULL;
```

When a schema change is made on the module and that module depends on another one,
it is necessary for correct dependency order to write the `depend.conf` file
that indicates the list of modules and their version we are depending on.

For example, if we update a module named `blogs` and that module depends on
`awa` which itself depends on `ado`, the `depend.conf` file must specify these
two modules with their specific versions.  For example:

```
awa:1 ado:2
```

It is also possible to create intermediate upgrade versions for the same module.
This is useful when there are incremental development steps but also when some
migration becomes too complex.  By spliting the complex migration in simpler
and small incremental steps, this will help when the migration must be executed
because it will be possible to execute one simple step at a time.
