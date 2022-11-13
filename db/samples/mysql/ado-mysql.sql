/* File generated automatically by dynamo */
/* Record representing a user */
CREATE TABLE IF NOT EXISTS user (
  /* the user identifier */
  `id` BIGINT UNIQUE NOT NULL,
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
INSERT IGNORE INTO ado_entity_type (name) VALUES
("user");
INSERT IGNORE INTO ado_version (name, version) VALUES ("ado", 2);
