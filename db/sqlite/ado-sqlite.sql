/* File generated automatically by dynamo */
/* Entity types */
CREATE TABLE entity_type (
  /* the entity type identifier */
  `ID` INTEGER PRIMARY KEY AUTOINCREMENT,
  /* the entity type name (table name) */
  `name` VARCHAR(127) UNIQUE NOT NULL
);
/* Sequence generator */
CREATE TABLE sequence (
  /* the sequence name */
  `name` VARCHAR(127) PRIMARY KEY,
  /* the sequence record version */
  `version` int ,
  /* the sequence value */
  `value` BIGINT ,
  /* the sequence block size */
  `block_size` BIGINT 
);
INSERT INTO entity_type (name) VALUES ("entity_type");
INSERT INTO entity_type (name) VALUES ("sequence");
