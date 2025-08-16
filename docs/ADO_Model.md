# Model Mapping

A big benefit when using ADO is the model mapping with the Ada and SQL code generator.

The model describes the database tables, their columns and relations with each others.
It is then used to generate the Ada implementation which provides operations to create,
update and delete records from the database and map them in Ada transparently.

The model can be defined in:

   * UML with a modeling tool that exports the model in XMI,
   * XML files following the [Hibernate](http://www.hibernate.org/) description,
   * YAML files according to the [Doctrine](https://www.doctrine-project.org/projects/doctrine-orm/en/2.6/reference/yaml-mapping.html) mapping.

This chapter focuses on the YAML description.

## Table definition

In YAML, the type definition follows the pattern below:

```
<table-type-name>:
  type: entity
  table: <table-name>
  description: <description>
  hasList: true|false
  indexes: 
  id:
  fields:
  oneToOne:
  oneToMany:
```

The *`table-type-name`* represents the Ada type name with the full package specification.
The code generator will add the `_Ref` prefix to the Ada type name to define the
final type with reference counting.  A private type is also generated with the
`_Impl` prefix.

The YAML fields have the following meanings:

| Field            | Description                                                                |
| ---------------- | ---------------------------------------------------------------            |
| type             | Must be 'entity' to describe a database table                              |
| table            | The name of the database table. This must be a valid SQL name              |
| description      | A comment description for the table and type definition                    |
| hasList          | When `true`, a `List` operation is also generated for the type             |
| indexes          | Defines the indexes for the table                                          |
| id               | Defines the primary keys for the table                                     |
| fields           | Defines the simple columns for the table                                   |
| oneToOne         | Defines the one to one table relations                                     |
| oneToMany        | Defines the one to many table relations                                    |


## Column mapping

Simple columns are represented within the `fields` section.

```
<table-type-name>:
  fields:
    <member-name>:
      type: <type>
      length: <length>
      description: <description>
      column: <column-name>
      not-null: true|false
      unique: true|false
      readonly: true|false
      version: false
```

The YAML fields have the following meanings:

| Field            | Description                                                                |
| ---------------- | ---------------------------------------------------------------            |
| type             | The column type. This type maps to an Ada type and an SQL type             |
| length           | For variable length columns, this is the maximum length of the column      |
| description      | A comment description for the table and type definition                    |
| column           | The database table column name                                              |
| not-null         | When true, indicates that the column cannot be null                        |
| unique           | When true, indicates that the column must be unique in the table rows      |
| readonly         | When true, the column cannot be updated. The `Save` operation will ignore updated.                                    |
| version          | Must be 'false' for simple columns                                         |

The `type` column describes the type of the column using a string that is agnostic of the
Ada and SQL languages.  The mapping of the type to SQL depends on the database.
The `not-null` definition has an impact on the Ada type since when the column can be null,
a special Ada type is required to represent that null value.

The `ADO.Nullable_X` types are all represented by the following record:

```
   type Nullable_X is record
      Value   : X := <default-value>;
      Is_Null : Boolean := True;
   end record;
```

The `Is_Null` boolean member must be checked to see if the value is null or not.
The comparison operation (`=`) ignores the `Value` comparison when one of the record
to compare has `Is_Null` set.


|Type    | not-null | SQL      | Ada      |
|----    | -------- | ----     | -----    |
|boolean | true     | TINYINT  | Boolean  |
|        | false    | TINYINT  | ADO.Nullable_Boolean  |
|byte    | true     | TINYINT  | -        |
|        | false    | TINYINT  | -        |
|integer | true     | INTEGER  | Integer  |
|        | false    | INTEGER  | ADO.Nullable_Integer  |
|long    | true     | BIGINT   | Long_Long_Integer |
|        | false    | BIGINT   | ADO.Nullable_Long_Integer |
|identifier |       | BIGINT   | ADO.Identifier    |
|entity_type| true  | INTEGER  | ADO.Entity_Type  |
|           | false | INTEGER  | ADO.Nullable_Entity_Type  |
|string  | true     | VARCHAR(N) | Unbounded_String |
|        | false    | VARCHAR(N) | ADO.Nullable_String |
|date    | true     | DATE       | Ada.Calendar.Time |
|        | false    | DATE       | ADO.Nullable_Time |
|time    | true     | DATETIME   | Ada.Calendar.Time |
|        | false    | DATETIME   | ADO.Nullable_Time |
|blob    |          | BLOB       | ADO.Blob_Ref      |

The `identifier` type is used to represent a foreign key mapped to a `BIGINT` in the database.
It is always represented by the Ada type `ADO.Identifier` and the null value is represented by
the special value `ADO.NO_IDENTIFIER`.

The `blob` type is represented by an Ada stream array held by a reference counted object.
The reference can be null.

The `entity_type` type allows to uniquely identify the type of a database entity.
Each database table is associated with an `entity_type` unique value.  Such value is
created statically when the database schema is created and populated in the database.
The `entity_type` values are maintained in the `entity_type` ADO database table.

## Primary keys

Primary keys are used to uniquely identify a row within a table.
For the ADO framework, only the identifier and string primary types are supported.

```
<table-type-name>:
  id:
    <member-name>:
      type: {identifier|string}
      length: <length>
      description: <description>
      column: <column-name>
      not-null: true
      unique: true
      version: false
      generator:
        strategy: {none|auto|sequence}
```

The `generator` section describes how the primary key is generated.

|Strategy  | description                                     |
|--------- | ----------------------------------------------- |
|none      | the primary key is managed by the application   |
|auto      | use the database auto increment support         |
|sequence  | use the ADO sequence generator                  |

## Relations

A one to many relation is described by the following YAML description:
```
<table-type-name>:
  oneToMany:
    <member-name>:
      type: <model-type>
      description: <description>
      column: <column-name>
      not-null: true|false
      readonly| true|false
```

This represents the foreign key and this YAML description is to be put in the table that holds it.

The `type` definition describes the type of object at the end of the relation.
This can be the `identifier` type which means the relation will not be strongly typed
and mapped to the `ADO.Identifier` type.  But it can be the table type name used for
another table definition.  In that case, the code generator will generate a getter
and setter that will use the object reference instance.

Circular dependencies are allowed within the same Ada package.  That is, two tables
can reference each other as long as they are defined in the same Ada package.
A relation can use a reference of a type declared in another YAML description from
another Ada package.  In that case, `with` clauses are generated to import them.

## Versions

Optimistic locking is a mechanism that allows updating the same database record
from several transactions without having to take a strong row lock that would block
transactions.  By having a version column that is incremented after each change,
it is possible to detect that the database row was modified when we want to update it.
When this happens, the optimistic lock exception `ADO.Objects.LAZY_LOCK` is raised
and it is the responsibility of the application to handle the failure by retrying
the update.

For the optimistic locking to work, a special integer based column must be declared.

```
<table-type-name>:
  fields:
    <member-name>:
      type: <type>
      description: <description>
      column: <column-name>
      not-null: true
      unique: false
      version: true
```

The generated Ada code gives access to the version value but it does not allow its modification.
The version column is incremented only by the `Save` procedure and only if at least one
field of the record was modified (otherwise the `Save` has no effect).
The version number starts with the value `1`.
## Objects
When a database table is mapped into an Ada object, the application holds a reference
to that object through the `Object_Ref` type.
The `Object_Ref` tagged type is the root type of any database record reference.
Reference counting is used so that the object can be stored, shared and the memory
management is handled automatically.  It defines generic operations to be able to:

  * load the database record and map it to the Ada object,
  * save the Ada object into the database either by inserting or updating it,
  * delete the database record.

The Dynamo code generator will generate a specific tagged type for each database table
that is mapped.  These tagged type will inherit from the `Object_Ref` and will implement
the required abstract operations.  For each of them, the code generator will generate
the `Get_X` and `Set_X` operation for each column mapped in Ada.

Before the `Object_Ref` is a reference, it does not hold the database record itself.
The `ADO.Objects.Object_Record` tagged record is used for that and it defines the
root type for the model representation.  The type provides operations to modify a
data field of the record while tracking its changes so that when the `Save` operation
is called, only the data fields that have been modified are updated in the database.
An application will not use nor access the `Object_Record`.  The Dynamo code generator
generates a private type to make sure it is only accessed through the reference.

Several predicate operations are available to help applications check the validity
of an object reference:

| Function    | Description |
| ----------- |--------------------------------------------------------- |
| Is_Null     | When returning True, it indicates the reference is NULL. |
| Is_Loaded   | When returning True, it indicates the object was loaded from the database. |
| Is_Inserted | When returning True, it indicates the object was inserted in the database. |
| Is_Modified | When returning True, it indicates the object was modified and must be saved. |

Let's assume we have a `User_Ref` mapped record, an instance of the reference would
be declared as follows:

```Ada
with Samples.User.Model;
...
  User : Samples.User.Model.User_Ref;
```

After this declaration, the reference is null and the following assumption is true:

```Ada
User.Is_Null and not User.Is_Loaded and not User.Is_Inserted
```

If we set a data field such as the name, an object is allocated and the reference
is no longer null.

```Ada
User.Set_Name ("Ada Lovelace");
```

After this statement, the following assumption is true:

```Ada
not User.Is_Null and not User.Is_Loaded and not User.Is_Inserted
```

With this, it is therefore possible to identify that this object is not yet
saved in the database.  After calling the `Save` procedure, a primary key is
allocated and the following assumption becomes true:

```Ada
not User.Is_Null and not User.Is_Loaded and User.Is_Inserted
```

## Loading Objects
Three operations are generated by the Dynamo code generator to help in loading
a object from the database: two `Load` procedures and a `Find` procedure.
The `Load` procedures are able to load an object by using its primary key.
Two forms of `Load` are provided: one that raises the `ADO.Objects.NOT_FOUND`
exception and another that returns an additional `Found` boolean parameter.
Within the application, if the database row is expected to exist, the first
form should be used.  In other cases, when the application expects that the
database record may not exist, the second form is easier and avoids raising
and handling an exception for a common case.

```Ada
 User.Load (Session, 1234);
```

The `Find` procedure allows to retrieve a database record by specifying a
filter.  The filter object is represented by the `ADO.SQL.Query` tagged record.
A simple query filter is declared as follows:

```Ada
Filter : ADO.SQL.Query;
```

The filter is an SQL fragment that is inserted within the `WHERE` clause to
find the object record.  The filter can use parameters that are configured
by using the `Bind_Param` or `Add_Param` operations.  For example, to find
a user from its name, the following filter could be set:

```Ada
Filter.Set_Filter ("name = :name");
Filter.Bind_Param ("name", "Ada Lovelace");
```

Once the query filter is initialized and configured with its parameters,
the `Find` procedure can be called:

```Ada
Found : Boolean;
...
User.Find (Session, Filter, Found);
```

The `Find` procedure does not raise an exception if the database record is not found.
Instead, it returns a boolean status in the `Found` output parameter.  The `Find`
procedure will execute an SQL `SELECT` statement with a `WHERE` clause to retrieve
the database record.  The `Found` output parameter is set when the query returns
exactly one row.

## Modifying Objects
To modify an object, applications will use one of the `Set_X` operation generated
for each mapped column.  The ADO runtime will keep track of which data fields are
modified.  The `Save` procedure must be called to update the database record.
When calling it, an SQL `UPDATE` statement is generated to update the modified
data fields.

```Ada
User.Set_Status (1);
User.Save (Session);
```

## Deleting Objects
Deleting objects is made by using the `Delete` operation.

```Ada
User.Delete (Session);
```

Sometimes you may want to delete an object without having to load it first.
This is possible by delete an object without loading it.  For this, set the
primary key on the object and call the `Delete` operation:

```Ada
User.Set_Id (42);
User.Delete (Session);
```

