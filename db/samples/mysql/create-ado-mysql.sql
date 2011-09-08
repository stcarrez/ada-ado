/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/mysql/ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(256) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT ,
  PRIMARY KEY (`name`)
);
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(256) UNIQUE NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("sequence")
,("entity_type")
;
/* Copied from ./db/samples/mysql/ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE user (
  /* the user identifier */
  `ID` BIGINT NOT NULL,
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
  `STATUS` Integer ,
  PRIMARY KEY (`ID`)
);
INSERT INTO entity_type (name) VALUES
("user")
;
