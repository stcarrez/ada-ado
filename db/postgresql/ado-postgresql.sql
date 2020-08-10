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
