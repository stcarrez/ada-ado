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
,("test_user")
;
