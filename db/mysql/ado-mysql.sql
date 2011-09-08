/* File generated automatically by dynamo */
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(256) NOT NULL,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT ,
  PRIMARY KEY (`name`)
);
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `id` INTEGER  AUTO_INCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(256) UNIQUE NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT INTO entity_type (name) VALUES
("sequence")
,("entity_type")
;
