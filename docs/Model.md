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
      type: identifier|string
      length: <length>
      description: <description>
      column: <column-name>
      not-null: true
      unique: true
      version: false
```

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
