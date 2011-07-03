/* File generated automatically by dynamo */
/* Record representing a user */
create table user (
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
  primary key (`ID`)
);
insert into entity_type (name) values
("user")
;
