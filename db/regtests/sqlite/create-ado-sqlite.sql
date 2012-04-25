/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL
);
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(127) PRIMARY KEY,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT 
);
INSERT INTO entity_type (name) VALUES ("entity_type");
INSERT INTO entity_type (name) VALUES ("sequence");
/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE TEST_COMMENTS (
  /* the comment identifier */
  `ID` INTEGER PRIMARY KEY,
  /* the comment version. */
  `version` int ,
  /* the comment publication date. */
  `DATE` TIMESTAMP NOT NULL,
  /* the comment message. */
  `MESSAGE` VARCHAR(256) NOT NULL,
  /* the entity identifier to which this comment is associated. */
  `ENTITY_ID` INTEGER ,
  /* the user who posted this comment */
  `USER_FK` INTEGER NOT NULL,
  /* the entity type that correspond to the entity associated with this comment. */
  `ENTITY__TYPE_FK` INTEGER NOT NULL
);
/* Record representing a user */
CREATE TABLE allocate (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
  /* the allocate version. */
  `version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) 
);
/*  */
CREATE TABLE test_image (
  /* the image identifier */
  `id` INTEGER PRIMARY KEY,
  /* the image version. */
  `version` int ,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the image data */
  `image` BLOB NOT NULL
);
/* Record representing a user */
CREATE TABLE test_user (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
  /* the comment version. */
  `version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the user name */
  `NAME` VARCHAR(255) ,
  /* the user name */
  `select` VARCHAR(255) 
);
INSERT INTO entity_type (name) VALUES ("TEST_COMMENTS");
INSERT INTO entity_type (name) VALUES ("allocate");
INSERT INTO entity_type (name) VALUES ("test_image");
INSERT INTO entity_type (name) VALUES ("test_user");
