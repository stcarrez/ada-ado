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
/* Copied from ./db/regtests/mysql/ado-mysql.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE TEST_COMMENTS (
  /*  */
  `ID` INTEGER ,
  /*  */
  `VERSION` int ,
  /*  */
  `DATE` TIMESTAMP NOT NULL,
  /*  */
  `MESSAGE` VARCHAR(256) NOT NULL,
  /*  */
  `ENTITY_ID` INTEGER ,
  /*  */
  `USER_FK` INTEGER NOT NULL,
  /*  */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE test_user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the user name */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE allocate (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
INSERT INTO entity_type (name) VALUES
("TEST_COMMENTS")
,("test_user")
,("allocate")
;
