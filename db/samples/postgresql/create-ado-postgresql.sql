/* Copied from ado-postgresql.sql*/
/* File generated automatically by dynamo */
/* Entity table that enumerates all known database tables */
CREATE TABLE IF NOT EXISTS entity_type (
  /* the database table unique entity index */
  "id" SERIAL,
  /* the database entity name */
  "name" VARCHAR(127) UNIQUE ,
  PRIMARY KEY ("id")
);
/* Sequence generator */
CREATE TABLE IF NOT EXISTS sequence (
  /* the sequence name */
  "name" VARCHAR(127) UNIQUE NOT NULL,
  /* the sequence record version */
  "version" INTEGER NOT NULL,
  /* the sequence value */
  "value" BIGINT NOT NULL,
  /* the sequence block size */
  "block_size" BIGINT NOT NULL,
  PRIMARY KEY ("name")
);
INSERT INTO entity_type (name) VALUES
('entity_type'), ('sequence')
  ON CONFLICT DO NOTHING;
/* Copied from ado-postgresql.sql*/
/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE IF NOT EXISTS user (
  /* the user identifier */
  "id" BIGINT UNIQUE NOT NULL,
  /*  */
  "object_version" INTEGER NOT NULL,
  /* the user name */
  "name" VARCHAR(256) NOT NULL,
  /* the user email */
  "email" VARCHAR(256) UNIQUE NOT NULL,
  /* the user registration date */
  "date" VARCHAR(256) NOT NULL,
  /* the user description */
  "description" VARCHAR(256) NOT NULL,
  /* the user status */
  "status" INTEGER NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO entity_type (name) VALUES
('user')
  ON CONFLICT DO NOTHING;
