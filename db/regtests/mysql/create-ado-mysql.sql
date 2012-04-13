/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  PRIMARY KEY (`id`)
);
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(127) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT ,
  PRIMARY KEY (`name`)
);
INSERT INTO entity_type (name) VALUES
("entity_type")
,("sequence")
;
/* Copied from ado-mysql.sql*/
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
CREATE TABLE allocate (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
/*  */
CREATE TABLE test_image (
  /*  */
  `id` INTEGER ,
  /*  */
  `version` int ,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `image` BLOB NOT NULL,
  PRIMARY KEY (`id`)
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
  /* the user name */
  `select` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
INSERT INTO entity_type (name) VALUES
("TEST_COMMENTS")
,("allocate")
,("test_image")
,("test_user")
;
