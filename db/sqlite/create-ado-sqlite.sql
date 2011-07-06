/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256) PRIMARY KEY,
  /* the sequence record version */
  `version` int NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT NOT NULL
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256) UNIQUE NOT NULL
);
insert into entity_type (name) values ("sequence");
insert into entity_type (name) values ("entity_type");
/* Copied from ./db/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256) PRIMARY KEY,
  /* the sequence record version */
  `version` int NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT NOT NULL
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256) UNIQUE NOT NULL
);
insert into entity_type (name) values ("sequence");
insert into entity_type (name) values ("entity_type");
