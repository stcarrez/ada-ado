# Objects
When a database table is represented by an Ada type, the `ADO.Objects.Object_Record`
tagged record is used as the root type for the model representation.  The type provides
operations to.

The `Object_Ref` type is the root type of any database record reference.

## Object Creation

## Object Deletion

## Sequence Generators
The sequence generator is responsible for creating unique ID's
across all database objects.

Each table can be associated with a sequence generator.
The sequence factory is shared by several sessions and the
implementation is thread-safe.

The `HiLoGenerator` implements a simple High Low sequence generator
by using sequences that avoid to access the database.

Example:

```Ada
F  : Factory;
Id : Identifier;
...
  Allocate (Manager => F, Name => "user", Id => Id);
```

### HiLo Sequence Generator
The HiLo sequence generator.  This sequence generator uses a database table
`sequence` to allocate blocks of identifiers for a given sequence name.
The sequence table contains one row for each sequence.  It keeps track of
the next available sequence identifier (in the `value column).

To allocate a sequence block, the HiLo generator obtains the next available
sequence identified and updates it by adding the sequence block size.  The
HiLo sequence generator will allocate the identifiers until the block is
full after which a new block will be allocated.



