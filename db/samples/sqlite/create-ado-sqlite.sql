/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(256) PRIMARY KEY,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT 
);
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(256) UNIQUE NOT NULL
);
INSERT INTO entity_type (name) VALUES ("sequence");
INSERT INTO entity_type (name) VALUES ("entity_type");
/* Copied from ./db/samples/sqlite/ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE user (
  /* the user identifier */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `object_version` int ,
  /* the user name */
  `NAME` VARCHAR(256) ,
  /* the user email */
  `EMAIL` VARCHAR(256) UNIQUE ,
  /* the user registration date */
  `DATE` VARCHAR(256) ,
  /* the user description */
  `DESCRIPTION` VARCHAR(256) ,
  /* the user status */
  `STATUS` Integer 
);
INSERT INTO entity_type (name) VALUES ("user");
