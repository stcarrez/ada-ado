/* File generated automatically by dynamo */
/*  */
create table TEST_COMMENTS (
  /*  */
  `ID` INTEGER,
  /*  */
  `VERSION` int,
  /*  */
  `DATE` TIMESTAMP,
  /*  */
  `MESSAGE` VARCHAR(256),
  /*  */
  `ENTITY_ID` INTEGER,
  /*  */
  `USER_FK` INTEGER,
  /*  */
  `ENTITY__TYPE_FK` INTEGER,
  primary key (`id`)
);
/* Record representing a user */
create table test_user (
  /* the user id */
  `ID` BIGINT,
  /*  */
  `object_version` int,
  /* the sequence value */
  `VALUE` BIGINT,
  /* the user name */
  `NAME` VARCHAR(255),
  primary key (`id`)
);
/* Record representing a user */
create table allocate (
  /* the user id */
  `ID` BIGINT,
  /*  */
  `object_version` int,
  /* the sequence value */
  `NAME` VARCHAR(255),
  primary key (`id`)
);
