/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8;;
INSERT INTO entity_type (name) VALUES
("entity_type")
,("sequence")
;
/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE user (
  /* the user identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `object_version` INTEGER NOT NULL,
  /* the user name */
  `name` VARCHAR(256) NOT NULL,
  /* the user email */
  `email` VARCHAR(256) UNIQUE NOT NULL,
  /* the user registration date */
  `date` VARCHAR(256) NOT NULL,
  /* the user description */
  `description` VARCHAR(256) NOT NULL,
  /* the user status */
  `status` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT INTO entity_type (name) VALUES
("user")
;
