pragma synchronous=OFF;
/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Entity table that enumerates all known database tables */
CREATE TABLE IF NOT EXISTS ado_entity_type (
  /* the database table unique entity index */
  `id` INTEGER  PRIMARY KEY AUTOINCREMENT,
  /* the database entity name */
  `name` VARCHAR(127) UNIQUE );
/* Sequence generator */
CREATE TABLE IF NOT EXISTS ado_sequence (
  /* the sequence name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the sequence record version */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `value` BIGINT NOT NULL,
  /* the sequence block size */
  `block_size` BIGINT NOT NULL,
  PRIMARY KEY (`name`)
);
/* Database schema version (per module) */
CREATE TABLE IF NOT EXISTS ado_version (
  /* the module name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the database version schema for this module */
  `version` INTEGER NOT NULL,
  PRIMARY KEY (`name`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_entity_type');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_sequence');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_version');
INSERT OR IGNORE INTO ado_version (name, version) VALUES ('ado', 2);
/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* This is the Audit_Info table */
CREATE TABLE IF NOT EXISTS audit_info (
  /*  */
  `id` BIGINT ,
  /* the entity id */
  `entity_id` BIGINT ,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  /* the old value */
  `old_value` VARCHAR(255) ,
  /* the new value */
  `new_value` VARCHAR(255) ,
  /* the audit date */
  `date` DATETIME NOT NULL,
  PRIMARY KEY (`id`)
);
/* This is the User email table */
CREATE TABLE IF NOT EXISTS audit_email (
  /*  */
  `id` BIGINT UNIQUE NOT NULL,
  /* the user email address */
  `user_email` VARCHAR(32) UNIQUE ,
  /* the user email status */
  `email_status` INTEGER ,
  /* the email date */
  `email_date` DATETIME ,
  /* the email creation date */
  `email_create_date` DATETIME NOT NULL,
  /* the email info */
  `email_info` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
);
/* This is a generic property */
CREATE TABLE IF NOT EXISTS audit_property (
  /*  */
  `id` VARCHAR(255) UNIQUE NOT NULL,
  /* the property value */
  `user_email` INTEGER ,
  /* a float property value */
  `float_value` FLOAT NOT NULL,
  /* a double property value */
  `double_value` DOUBLE NOT NULL,
  /* the property entity type */
  `kind` INTEGER NOT NULL,
  /* the optional property entity type */
  `optional_kind` INTEGER ,
  /* the optional object_id */
  `object_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE IF NOT EXISTS TEST_COMMENTS (
  /* the comment identifier */
  `ID` INTEGER ,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* the comment publication date. */
  `DATE` TIMESTAMP NOT NULL,
  /* the comment message. */
  `MESSAGE` VARCHAR(256) NOT NULL,
  /* the entity identifier to which this comment is associated. */
  `ENTITY_ID` INTEGER NOT NULL,
  /* the user who posted this comment */
  `USER_FK` INTEGER NOT NULL,
  /* the entity type that correspond to the entity associated with this comment. */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
);
/*  */
CREATE TABLE IF NOT EXISTS test_image (
  /* the image identifier */
  `id` INTEGER ,
  /* the image version. */
  `version` INTEGER NOT NULL,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the image data */
  `image` LONGBLOB ,
  PRIMARY KEY (`id`)
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS allocate (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the allocate version. */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_keys (
  /* the user id */
  `ID` VARCHAR(255) UNIQUE NOT NULL,
  /* the allocate version. */
  `version` INTEGER NOT NULL,
  /* the name */
  `NAME` VARCHAR(255) ,
  /* the cost */
  `cost` FLOAT NOT NULL,
  /* the total */
  `total` DOUBLE NOT NULL,
  /* the user */
  `user_id` BIGINT NOT NULL,
  /* the second key */
  `key_id` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_user (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the user name */
  `NAME` VARCHAR(255) ,
  /* the user name */
  `select` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_nullable_table (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* an identifier value */
  `ID_VALUE` BIGINT ,
  /* an integer value */
  `INT_VALUE` INTEGER ,
  /* a boolean value */
  `BOOL_VALUE` TINYINT ,
  /* a string value */
  `STRING_VALUE` VARCHAR(255) ,
  /* a time value */
  `TIME_VALUE` DATETIME ,
  /* an entity value */
  `ENTITY_VALUE` INTEGER ,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_table (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* an identifier value */
  `ID_VALUE` BIGINT NOT NULL,
  /* an integer value */
  `INT_VALUE` INTEGER NOT NULL,
  /* a boolean value */
  `BOOL_VALUE` TINYINT NOT NULL,
  /* a string value */
  `STRING_VALUE` VARCHAR(255) NOT NULL,
  /* a time value */
  `TIME_VALUE` DATETIME NOT NULL,
  /* an entity value */
  `ENTITY_VALUE` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('audit_info');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('audit_email');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('audit_property');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('TEST_COMMENTS');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('test_image');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('allocate');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('test_keys');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('test_user');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('test_nullable_table');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('test_table');
INSERT OR IGNORE INTO ado_version (name, version) VALUES ('ado', 2);
