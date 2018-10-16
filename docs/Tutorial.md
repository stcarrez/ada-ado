# Tutorial

This small tutorial explains how an application can access
a database (MySQL or SQLite) to store its data by using the
Ada Database Objects framework.  The framework has several similarities
with the excellent [Hibernate](http://www.hibernate.org/) Java framework.

The ADO framework is composed of:

   * A code generator provided by [Dynamo](https://github.com/stcarrez/dynamo),
   * A core runtime,
   * A set of database drivers (PostgreSQL, MySQL, SQLite).

The application is a simple user management database which has only one table.

## Defining the data model

The first step is to design the data model.  For this we will write
a XML mapping file that describes the data table as well as how the different
columns are mapped to an Ada type.  ADO uses (almost) the same mapping
definition as [Hibernate](http://www.hibernate.org/).  Modeling tools such as
[AndroMDA](http://www.andromda.org) are able to generate these XML mapping files from UML models.

Let's define a mapping for a simple `user` table and save it in `db/user.hbm.xml`:
```
<hibernate-mapping>
  <class name="Samples.User.Model.User" table="user">
    <comment>Record representing a user</comment>
    <id name="id" type="ADO.Identifier">
       <comment>the user identifier</comment>
       <column name="ID" not-null="true" unique="true" sql-type="BIGINT"/>
       <generator class="sequence"/>
    </id>
    <version name="version" type="int" column="object_version"/>
    <property name="name" type="String">
        <comment>the user name</comment>
        <column name="NAME" not-null="false" unique="false" sql-type="VARCHAR(256)"/>
    </property>
    <property name="date" type="String">
        <comment>the user registration date</comment>
        <column name="DATE" not-null="false" unique="false" sql-type="VARCHAR(256)"/>
    </property>
  </class>
</hibernate-mapping>
```

This mapping indicates that the database table `user` is represented by the `User` tagged record
declared in the `Samples.User.Model` package.  The table contains a `name` and a `date` column members
which are both a string.  The table primary key is represented by the `id` column.

## Generating the Ada model

The [Dynamo](https://github.com/stcarrez/dynamo) code generator is then used
to generate the package and Ada records that represent our data model.

```
dynamo generate db
```

The generator will build the package specification and body for
`Samples.User.Model` package.  The files are created in `src/model` to make it
clear that these files are model files that are generated.  The database table `user` is represented
by the Ada tagged record `User_Ref`.  The record members are not visible and to access the attributes
it is necessary to use getter or setter operations.

## Getting a Database Connection

To access the database, we will need a database connection.
These connections are obtained from a factory and they are represented
by a session object.

The session factory is the entry point to obtain a database session.

```
with ADO.Sessions;
with ADO.Sessions.Factory;
   ...
   Factory : ADO.Sessions.Factory.Session_Factory;
```

The factory can be initialized by giving a URI string that identifies the
driver and the information to connect to the database.  Once created,
the factory return session objects to connect to that database.  To connect
to another database, another factory is necessary.

To get access to a MySQL database, the factory could be initialized as follows:

```
   ADO.Sessions.Factory.Create (Factory, "mysql://localhost:3306/ado_test?user=test");
```

And to use an SQLite database, you could use:

```
   ADO.Sessions.Factory.Create (Factory, "sqlite:///tests.db");
```

Factory initialization is done once when an application starts.  The same
factory object can be used by multiple tasks.

### Opening a Session

The session is created by using the `Get_Session` or the `Get_Master_Session`
function of the factory.  Both function return a session object
associated with a database connection.  The `Get_Session` will return
a `Session` object which is intended to provide read-only access to the
database.  The `Get_Master_session` returns a `Master_Session` object
which provides a read-write access to the database.

In a typical MySQL Master/Slave replication, the `Master_Session` will refer
to a connection to the MySQL master while the `Session` will refer to a slave.
With an SQLite database, both sessions will in fact share the same SQLite internal
connection.

To load or save the user object in the database, we need a `Master_Session`  database connection:

```
with ADO.Sessions;
...
  Session : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
```


## Creating a database record

To create our first database record, we will declare a variable that will represent the new database record.
The `User_Ref` represents a reference to such record.

```
with Samples.User.Model;
  ...
  User : Samples.User.Model.User_Ref
```

After this declaration, the variable does not refer to any database record but we can still set some fields:

```
  User.Set_Name ("Harry");
  User.Set_Age (17);
```

To save the object in the database, we just need to call the `Save` operation.  To save the object, we need
a database session that is capable of updating and inserting new rows.  If the object does not yet have a primary key,
and if the primary key allocation mode is set to `hilo`, the ADO runtime will allocate a new unique primary key
before inserting the new row.

```
  User.Save (Session);
```

The primary key can be obtained after the first `Save` with the following operation:

```
  Id : ADO.Identifier := User.Get_Id;
```

## Loading a database record

Loading a database record is quite easy and the ADO framework proposes two mechanisms.
First, let's declare our user variable:

```
  Harry : User_Ref;
```

Then we can load the user record from the primary key identifier (assuming the identifier is ''23''):

```
  Harry.Load (Session, 23);
```

If the user cannot be found, the `Load` operation will raise the `NOT_FOUND` exception.

In many cases, we may not know the primary key but a search on one or several columns may be necessary.
For this, we can create a query filter and use the `Find` operation.  To use a query filter, we need
first to declare a `Query` object:

```
with ADO.SQL;
...
  Query : ADO.SQL.Query;
```

On the query object, we have to define the filter which represents the
condition and set the possible parameters used by the filter.

```
  Query.Bind_Param (1, "Harry");
  Query.Set_Filter ("name = ?"); 
```

Once the query is defined and initialized, we can find the database record:

```
  Found : Boolean;
...
  User.Find (Session, Query, Found);
```

Unlike the `Load` operation, `Find` does not raise an exception but instead returns a boolean value
telling whether the record was found or not.  The database query (and filter) has to return exactly one
record to consider the object as found.

## Getting a list of objects

When several records have to be read, it is necessary to use the `List` operation together with
a vector object.

```
  Users : User_Vector;
```

The `List` operation gets the vector object, the database session
and the query filter.  If the vector contained some elements, they
are removed and replaced by the query result.
```
  List (Users, Session, Query);
```

## Running SQL queries

Sometimes it is necessary to execute SQL queries to be able to get the result without having it
to be mapped to an Ada record.  For this, we are going to use the `ADO.Statements`

```
with ADO.Statements;
   ...
   Statement : ADO.Statements.Query_Statement := Session.Create_Statement ("select count(*) from user");
```

and then execute it and retrieve the result.

```
   Statement.Execute;
   if not Statement.Has_Elements then
      Put_Line ("SQL count() failed")
   else
      Put_Line (Integer'Image (Statement.Get_Integer (0)));
   end if;
```

