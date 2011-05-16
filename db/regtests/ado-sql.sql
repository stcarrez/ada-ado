/* File generated automatically by dynamo */
/*  */
create table TEST_COMMENTS (
  /*  */
  `ID` INTEGER NOT NULL,
  /*  */
  `VERSION` int NOT NULL,
  /*  */
  `DATE` TIMESTAMP NOT NULL,
  /*  */
  `MESSAGE` VARCHAR(256) NOT NULL,
  /*  */
  `ENTITY_ID` INTEGER NOT NULL,
  /*  */
  `USER_FK` INTEGER NOT NULL,
  /*  */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  primary key (`id`)
);
/* Record representing a user */
create table test_user (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the user name */
  `NAME` VARCHAR(255) NOT NULL,
  primary key (`id`)
);
/* Record representing a user */
create table allocate (
  /* the user id */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int NOT NULL,
  /* the sequence value */
  `NAME` VARCHAR(255) NOT NULL,
  primary key (`id`)
);
insert into entity_type (name) values
("TEST_COMMENTS")
,("test_user")
,("allocate")
;
