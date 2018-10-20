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
