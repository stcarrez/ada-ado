/* File generated automatically by dynamo */
/*  */
create table TEST_COMMENTS (
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
  primary key (`ID`)
);
/* Record representing a user */
create table test_user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the user name */
  `NAME` VARCHAR(255) ,
  primary key (`ID`)
);
/* Record representing a user */
create table allocate (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int ,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  primary key (`ID`)
);
insert into entity_type (name) values
("TEST_COMMENTS")
,("test_user")
,("allocate")
;
