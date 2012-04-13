/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
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
("sequence")
;
/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE user (
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
  PRIMARY KEY (`ID`)
);
INSERT INTO entity_type (name) VALUES
("user")
;
