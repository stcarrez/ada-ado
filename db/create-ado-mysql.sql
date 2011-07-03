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
/* Copied from ./db/ado-mysql.sql*/
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
