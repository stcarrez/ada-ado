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
/* Copied from ./db/samples/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
create table user (
  /* the user identifier */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `object_version` int NOT NULL,
  /* the user name */
  `NAME` VARCHAR(256) NOT NULL,
  /* the user email */
  `EMAIL` VARCHAR(256) UNIQUE NOT NULL,
  /* the user registration date */
  `DATE` VARCHAR(256) NOT NULL,
  /* the user description */
  `DESCRIPTION` VARCHAR(256) NOT NULL,
  /* the user status */
  `STATUS` Integer NOT NULL
);
insert into entity_type (name) values ("user");
