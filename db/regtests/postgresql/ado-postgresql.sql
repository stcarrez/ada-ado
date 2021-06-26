/* File generated automatically by dynamo */
/* This is the Audit_Info table */
CREATE TABLE IF NOT EXISTS audit_info (
  /*  */
  "id" BIGINT ,
  /* the entity id */
  "entity_id" BIGINT ,
  /* the entity type */
  "entity_type" INTEGER NOT NULL,
  /* the old value */
  "old_value" VARCHAR(255) ,
  /* the new value */
  "new_value" VARCHAR(255) ,
  /* the audit date */
  "date" DATE NOT NULL,
  PRIMARY KEY ("id")
);
/* This is the User email table */
CREATE TABLE IF NOT EXISTS audit_email (
  /*  */
  "id" BIGINT UNIQUE NOT NULL,
  /* the user email address */
  "user_email" VARCHAR(32) UNIQUE ,
  /* the user email status */
  "email_status" INTEGER ,
  /* the email date */
  "email_date" DATE ,
  /* the email creation date */
  "email_create_date" DATE NOT NULL,
  /* the email info */
  "email_info" VARCHAR(255) NOT NULL,
  PRIMARY KEY ("id")
);
/* This is a generic property */
CREATE TABLE IF NOT EXISTS audit_property (
  /*  */
  "id" VARCHAR(255) UNIQUE NOT NULL,
  /* the property value */
  "user_email" INTEGER ,
  /* a float property value */
  "float_value" FLOAT NOT NULL,
  /* a double property value */
  "double_value" DOUBLE NOT NULL,
  /* the property entity type */
  "kind" INTEGER NOT NULL,
  /* the optional property entity type */
  "optional_kind" INTEGER ,
  /* the optional object_id */
  "object_id" BIGINT ,
  PRIMARY KEY ("id")
);
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE IF NOT EXISTS TEST_COMMENTS (
  /* the comment identifier */
  "id" INTEGER ,
  /* the comment version. */
  "version" INTEGER NOT NULL,
  /* the comment publication date. */
  "date" TIMESTAMP NOT NULL,
  /* the comment message. */
  "message" VARCHAR(256) NOT NULL,
  /* the entity identifier to which this comment is associated. */
  "entity_id" INTEGER NOT NULL,
  /* the user who posted this comment */
  "user_fk" INTEGER NOT NULL,
  /* the entity type that correspond to the entity associated with this comment. */
  "entity__type_fk" INTEGER NOT NULL,
  PRIMARY KEY ("id")
);
/*  */
CREATE TABLE IF NOT EXISTS test_image (
  /* the image identifier */
  "id" INTEGER ,
  /* the image version. */
  "version" INTEGER NOT NULL,
  /* the message creation date */
  "create_date" TIMESTAMP NOT NULL,
  /* the image data */
  "image" BYTEA ,
  PRIMARY KEY ("id")
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS allocate (
  /* the user id */
  "id" BIGINT UNIQUE NOT NULL,
  /* the allocate version. */
  "version" INTEGER NOT NULL,
  /* the sequence value */
  "name" VARCHAR(255) ,
  PRIMARY KEY ("id")
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_keys (
  /* the user id */
  "id" VARCHAR(255) UNIQUE NOT NULL,
  /* the allocate version. */
  "version" INTEGER NOT NULL,
  /* the name */
  "name" VARCHAR(255) ,
  /* the cost */
  "cost" FLOAT NOT NULL,
  /* the total */
  "total" DOUBLE NOT NULL,
  /* the user */
  "user_id" BIGINT NOT NULL,
  /* the second key */
  "key_id" VARCHAR(255) NOT NULL,
  PRIMARY KEY ("id")
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_user (
  /* the user id */
  "id" BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  "version" INTEGER NOT NULL,
  /* the sequence value */
  "value" BIGINT NOT NULL,
  /* the user name */
  "name" VARCHAR(255) ,
  /* the user name */
  "select" VARCHAR(255) ,
  PRIMARY KEY ("id")
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_nullable_table (
  /* the user id */
  "id" BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  "version" INTEGER NOT NULL,
  /* an identifier value */
  "id_value" BIGINT ,
  /* an integer value */
  "int_value" INTEGER ,
  /* a boolean value */
  "bool_value" BOOLEAN ,
  /* a string value */
  "string_value" VARCHAR(255) ,
  /* a time value */
  "time_value" TIMESTAMP ,
  /* an entity value */
  "entity_value" INTEGER ,
  PRIMARY KEY ("id")
);
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_table (
  /* the user id */
  "id" BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  "version" INTEGER NOT NULL,
  /* an identifier value */
  "id_value" BIGINT NOT NULL,
  /* an integer value */
  "int_value" INTEGER NOT NULL,
  /* a boolean value */
  "bool_value" BOOLEAN NOT NULL,
  /* a string value */
  "string_value" VARCHAR(255) NOT NULL,
  /* a time value */
  "time_value" TIMESTAMP NOT NULL,
  /* an entity value */
  "entity_value" INTEGER NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO entity_type (name) VALUES
('audit_info'), ('audit_email'), ('audit_property'), ('TEST_COMMENTS'), ('test_image'), ('allocate'), ('test_keys'), ('test_user'), ('test_nullable_table'), ('test_table')
  ON CONFLICT DO NOTHING;
