/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE TEST_COMMENTS (
  /* the comment identifier */
  `ID` INTEGER ,
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
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
);
/* Record representing a user */
CREATE TABLE allocate (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /* the allocate version. */
  `version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
);
/*  */
CREATE TABLE test_image (
  /* the image identifier */
  `id` INTEGER ,
  /* the image version. */
  `version` int ,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the image data */
  `image` BLOB NOT NULL,
  PRIMARY KEY (`id`)
);
/* Record representing a user */
CREATE TABLE test_user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /* the comment version. */
  `version` int ,
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
