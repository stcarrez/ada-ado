/* File generated automatically by dynamo */
/* Record representing a user */
create table user (
  /* the user identifier */
  `ID` BIGINT PRIMARY KEY,
  /*  */
  `object_version` int NOT NULL,
  /* the user name */
  `NAME` VARCHAR(256) NOT NULL,
  /* the user email */
  `EMAIL` VARCHAR(256) UNIQUE NOT NULL,
  /* the user registration date */
  `DATE` VARCHAR(256) NOT NULL,
  /* the user description */
  `DESCRIPTION` VARCHAR(256) NOT NULL,
  /* the user status */
  `STATUS` Integer NOT NULL
);
insert into entity_type (name) values ("user");
