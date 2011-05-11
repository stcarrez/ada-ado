/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256),
  /* the sequence record version */
  `version` int,
  /* the sequence value */
  `VALUE` BIGINT,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT,
  primary key (`name`)
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256),
  primary key (`id`)
);
