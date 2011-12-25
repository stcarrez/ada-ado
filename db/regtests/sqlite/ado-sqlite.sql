/* File generated automatically by dynamo */
/*  */
CREATE TABLE TEST_COMMENTS (
  /*  */
  `ID` INTEGER PRIMARY KEY,
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
  `ENTITY__TYPE_FK` INTEGER NOT NULL
);
/* Record representing a user */
CREATE TABLE allocate (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) 
);
/* Record representing a user */
CREATE TABLE test_user (
  /* the user id */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the user name */
  `NAME` VARCHAR(255) ,
  /* the user name */
  `select` VARCHAR(255) 
);
INSERT INTO entity_type (name) VALUES ("TEST_COMMENTS");
INSERT INTO entity_type (name) VALUES ("allocate");
INSERT INTO entity_type (name) VALUES ("test_user");
