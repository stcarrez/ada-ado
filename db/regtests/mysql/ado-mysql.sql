/* File generated automatically by dynamo */
/* This is the Audit_Info table */
CREATE TABLE IF NOT EXISTS audit_info (
  /*  */
  `id` BIGINT ,
  /* the entity id */
  `entity_id` BIGINT ,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  /* the old value */
  `old_value` VARCHAR(255) BINARY ,
  /* the new value */
  `new_value` VARCHAR(255) BINARY ,
  /* the audit date */
  `date` DATETIME NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* This is the User email table */
CREATE TABLE IF NOT EXISTS audit_email (
  /*  */
  `id` BIGINT UNIQUE NOT NULL,
  /* the user email address */
  `user_email` VARCHAR(32) BINARY UNIQUE ,
  /* the user email status */
  `email_status` INTEGER ,
  /* the email date */
  `email_date` DATETIME ,
  /* the email creation date */
  `email_create_date` DATETIME NOT NULL,
  /* the email info */
  `email_info` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* This is a generic property */
CREATE TABLE IF NOT EXISTS audit_property (
  /*  */
  `id` VARCHAR(255) BINARY UNIQUE NOT NULL,
  /* the property value */
  `user_email` INTEGER ,
  /* a float property value */
  `float_value` FLOAT NOT NULL,
  /* a double property value */
  `double_value` DOUBLE NOT NULL,
  /* the property entity type */
  `kind` INTEGER NOT NULL,
  /* the optional property entity type */
  `optional_kind` INTEGER ,
  /* the optional object_id */
  `object_id` BIGINT ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The Comment table records a user comment associated with a database entity.
                 The comment can be associated with any other database record. */
CREATE TABLE IF NOT EXISTS TEST_COMMENTS (
  /* the comment identifier */
  `ID` INTEGER ,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* the comment publication date. */
  `DATE` TIMESTAMP NOT NULL,
  /* the comment message. */
  `MESSAGE` VARCHAR(256) NOT NULL,
  /* the entity identifier to which this comment is associated. */
  `ENTITY_ID` INTEGER NOT NULL,
  /* the user who posted this comment */
  `USER_FK` INTEGER NOT NULL,
  /* the entity type that correspond to the entity associated with this comment. */
  `ENTITY__TYPE_FK` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS test_image (
  /* the image identifier */
  `id` INTEGER ,
  /* the image version. */
  `version` INTEGER NOT NULL,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the image data */
  `image` LONGBLOB ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Record representing a user */
CREATE TABLE IF NOT EXISTS allocate (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the allocate version. */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `NAME` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_user (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `VALUE` BIGINT NOT NULL,
  /* the user name */
  `NAME` VARCHAR(255) ,
  /* the user name */
  `select` VARCHAR(255) ,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_nullable_table (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* an identifier value */
  `ID_VALUE` BIGINT ,
  /* an integer value */
  `INT_VALUE` INTEGER ,
  /* a boolean value */
  `BOOL_VALUE` TINYINT ,
  /* a string value */
  `STRING_VALUE` VARCHAR(255) ,
  /* a time value */
  `TIME_VALUE` DATETIME ,
  /* an entity value */
  `ENTITY_VALUE` INTEGER ,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Record representing a user */
CREATE TABLE IF NOT EXISTS test_table (
  /* the user id */
  `ID` BIGINT UNIQUE NOT NULL,
  /* the comment version. */
  `version` INTEGER NOT NULL,
  /* an identifier value */
  `ID_VALUE` BIGINT NOT NULL,
  /* an integer value */
  `INT_VALUE` INTEGER NOT NULL,
  /* a boolean value */
  `BOOL_VALUE` TINYINT NOT NULL,
  /* a string value */
  `STRING_VALUE` VARCHAR(255) NOT NULL,
  /* a time value */
  `TIME_VALUE` DATETIME NOT NULL,
  /* an entity value */
  `ENTITY_VALUE` INTEGER NOT NULL,
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO entity_type (name) VALUES
("audit_info"), ("audit_email"), ("audit_property"), ("TEST_COMMENTS"), ("test_image"), ("allocate"), ("test_user"), ("test_nullable_table"), ("test_table");
