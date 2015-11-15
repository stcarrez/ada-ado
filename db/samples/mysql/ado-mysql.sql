/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE user (
  /* the user identifier */
  `ID` BIGINT NOT NULL,
  /*  */
  `object_version` int NOT NULL,
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
