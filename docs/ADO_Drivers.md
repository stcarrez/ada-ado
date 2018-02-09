# Database Drivers
The <b>ADO.Drivers</b> package represents the database driver that will create
database connections and provide the database specific implementation.  The driver
is either statically linked to the application and can be loaded dynamically if it was
built as a shared library.  For a dynamic load, the driver shared library name must be
prefixed by <b>libada_ado_</b>.  For example, for a <tt>mysql</tt> driver, the shared
library name is <tt>libada_ado_mysql.so</tt>.

## Initialization
The <b>ADO</b> runtime must be initialized by calling one of the <b>Initialize</b> operation.
A property file contains the configuration for the database drivers and the database
connection properties.

```Ada
ADO.Drivers.Initialize ("db.properties");
```

Once initialized, a configuration property can be retrieved by using the <tt>Get_Config</tt>
operation.

```Ada
URI : constant String := ADO.Drivers.Get_Config ("ado.database");
```

