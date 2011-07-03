/* Copied from /home/ciceron/work/pam/pam/awa/ado/db/ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Sequence generator */
create table sequence (
  /* the sequence name */
  `NAME` VARCHAR(256) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `VALUE` BIGINT ,
  /* the sequence block size */
  `BLOCK_SIZE` BIGINT ,
  primary key (`NAME`)
);
/* Entity types */
create table entity_type (
  /* the entity type identifier */
  `ID` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `NAME` VARCHAR(256) UNIQUE NOT NULL,
  primary key (`ID`)
);
insert into entity_type (name) values
("sequence")
,("entity_type")
;
/* Copied from ./db/samples/ado-mysql.sql*/
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
