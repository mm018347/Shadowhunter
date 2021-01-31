-- Adminer 4.7.8 MySQL dump

SET NAMES utf8;
SET time_zone = '+00:00';
SET foreign_key_checks = 0;
SET sql_mode = 'NO_AUTO_VALUE_ON_ZERO';

CREATE DATABASE `shadowhunter` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `shadowhunter`;

CREATE TABLE `action` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `roomround_id` bigint(20) unsigned DEFAULT NULL,
  `actioner_id` bigint(20) unsigned DEFAULT NULL,
  `content` varchar(20) DEFAULT NULL,
  `mtype` varchar(4) DEFAULT NULL,
  `action_flags` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  `actionee_id` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `action_roomround_id` (`roomround_id`),
  KEY `action_actioner_id` (`actioner_id`),
  KEY `action_actionee_id` (`actionee_id`)
) ENGINE=InnoDB AUTO_INCREMENT=724875 DEFAULT CHARSET=utf8;


CREATE TABLE `adminmanage` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `param_name` varchar(256) DEFAULT NULL,
  `param_value` varchar(256) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;

INSERT INTO `adminmanage` (`id`, `param_name`, `param_value`, `updatedat`, `createdat`) VALUES
(1,	'room_start',	'0',	'2016-05-20 15:01:44',	'2016-05-20 15:01:44'),
(2,	'room_end',	'24',	'2016-05-20 15:01:44',	'2016-05-20 15:01:44'),
(3,	'room_count',	'10',	'2019-10-28 00:18:01',	'2016-05-20 15:01:44'),
(4,	'admin_ip',	'127.0.0.1;',	'2016-05-20 15:01:44',	'2016-05-20 15:01:44')
ON DUPLICATE KEY UPDATE `id` = VALUES(`id`), `param_name` = VALUES(`param_name`), `param_value` = VALUES(`param_value`), `updatedat` = VALUES(`updatedat`), `createdat` = VALUES(`createdat`);

CREATE TABLE `article` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `thread_id` bigint(20) unsigned DEFAULT NULL,
  `uname` varchar(20) DEFAULT NULL,
  `handle_name` varchar(20) DEFAULT NULL,
  `trip` varchar(20) DEFAULT NULL,
  `password` varchar(20) DEFAULT NULL,
  `content` varchar(3000) DEFAULT NULL,
  `deleted` tinyint(1) DEFAULT NULL,
  `edited_times` int(11) DEFAULT NULL,
  `ip_address` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `article_thread_id` (`thread_id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;


CREATE TABLE `articlethread` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `handle_name` varchar(20) DEFAULT NULL,
  `trip` varchar(20) DEFAULT NULL,
  `title` varchar(80) DEFAULT NULL,
  `articles` int(11) DEFAULT NULL,
  `deleted` tinyint(1) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8;


CREATE TABLE `cardpool` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `card_no` int(11) DEFAULT NULL,
  `card_type` varchar(1) DEFAULT NULL,
  `card` varchar(4) DEFAULT NULL,
  `owner_id` bigint(20) unsigned DEFAULT NULL,
  `discarded` tinyint(1) DEFAULT NULL,
  `flags` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  `room_id` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `cardpool_owner_id` (`owner_id`),
  KEY `cardpool_room_id` (`room_id`)
) ENGINE=InnoDB AUTO_INCREMENT=307189 DEFAULT CHARSET=utf8;


CREATE TABLE `room` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `room_name` varchar(20) DEFAULT NULL,
  `room_comment` varchar(60) DEFAULT NULL,
  `max_user` int(11) DEFAULT NULL,
  `move_time` int(11) DEFAULT NULL,
  `action_time` int(11) DEFAULT NULL,
  `reaction_time` int(11) DEFAULT NULL,
  `room_arrange` varchar(10) DEFAULT NULL,
  `room_flags` varchar(1000) DEFAULT NULL,
  `whitecard_index` int(11) DEFAULT NULL,
  `blackcard_index` int(11) DEFAULT NULL,
  `greencard_index` int(11) DEFAULT NULL,
  `status` varchar(1) DEFAULT NULL,
  `victory` varchar(1) DEFAULT NULL,
  `victory_all` varchar(20) DEFAULT NULL,
  `ip_address` varchar(20) DEFAULT NULL,
  `talk_time` datetime DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6260 DEFAULT CHARSET=utf8;


CREATE TABLE `roomphase` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `roomround_id` bigint(20) unsigned DEFAULT NULL,
  `phase_no` int(11) DEFAULT NULL,
  `phase_type` varchar(2) DEFAULT NULL,
  `phase_subtype` varchar(4) DEFAULT NULL,
  `player` bigint(20) unsigned DEFAULT NULL,
  `deadline` datetime DEFAULT NULL,
  `additional` int(11) DEFAULT NULL,
  `phase_flags` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `roomphase_roomround_id` (`roomround_id`),
  KEY `roomphase_player` (`player`)
) ENGINE=InnoDB AUTO_INCREMENT=627779 DEFAULT CHARSET=utf8;


CREATE TABLE `roomround` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `room_id` bigint(20) unsigned DEFAULT NULL,
  `last_round` bigint(20) unsigned DEFAULT NULL,
  `round_no` int(11) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `roomround_room_id` (`room_id`),
  KEY `roomround_last_round` (`last_round`)
) ENGINE=InnoDB AUTO_INCREMENT=48572 DEFAULT CHARSET=utf8;


CREATE TABLE `talk` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `roomround_id` bigint(20) unsigned DEFAULT NULL,
  `actioner_id` bigint(20) unsigned DEFAULT NULL,
  `cssclass` varchar(20) DEFAULT NULL,
  `message` varchar(600) DEFAULT NULL,
  `mtype` varchar(4) DEFAULT NULL,
  `message_flags` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  `actionee_id` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `talk_roomround_id` (`roomround_id`),
  KEY `talk_actioner_id` (`actioner_id`),
  KEY `talk_actionee_id` (`actionee_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1835903 DEFAULT CHARSET=utf8;


CREATE TABLE `userentry` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `room_id` bigint(20) unsigned DEFAULT NULL,
  `user_id` bigint(20) unsigned DEFAULT NULL,
  `user_icon_id` bigint(20) unsigned DEFAULT NULL,
  `user_no` int(11) DEFAULT NULL,
  `uname` varchar(20) DEFAULT NULL,
  `handle_name` varchar(20) DEFAULT NULL,
  `trip` varchar(20) DEFAULT NULL,
  `password` varchar(20) DEFAULT NULL,
  `sex` varchar(1) DEFAULT NULL,
  `role_c` varchar(10) DEFAULT NULL,
  `subrole` varchar(10) DEFAULT NULL,
  `haterole` varchar(10) DEFAULT NULL,
  `damaged` int(11) DEFAULT NULL,
  `location` varchar(2) DEFAULT NULL,
  `action_point` int(11) DEFAULT NULL,
  `beads` int(2) DEFAULT NULL,
  `cash` int(11) DEFAULT NULL,
  `live` tinyint(1) DEFAULT NULL,
  `last_words` varchar(600) DEFAULT NULL,
  `revealed` tinyint(1) DEFAULT NULL,
  `won` tinyint(1) DEFAULT NULL,
  `revoked` tinyint(1) DEFAULT NULL,
  `ip_address0` varchar(20) DEFAULT NULL,
  `ip_address` varchar(20) DEFAULT NULL,
  `ip_address_md5` varchar(34) DEFAULT NULL,
  `last_round_no` int(11) DEFAULT NULL,
  `reaction` tinyint(1) DEFAULT NULL,
  `last_talk` varchar(600) DEFAULT NULL,
  `target_user` bigint(20) unsigned DEFAULT NULL,
  `room_flags` varchar(20) DEFAULT NULL,
  `role_flags` varchar(20) DEFAULT NULL,
  `user_flags` varchar(80) DEFAULT NULL,
  `card_flags` varchar(20) DEFAULT NULL,
  `getrole_flags` varchar(40) DEFAULT NULL,
  `item_flags` varchar(80) DEFAULT NULL,
  `item_preferred` varchar(10) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `userentry_room_id` (`room_id`),
  KEY `userentry_user_id` (`user_id`),
  KEY `userentry_user_icon_id` (`user_icon_id`),
  KEY `userentry_target_user` (`target_user`)
) ENGINE=InnoDB AUTO_INCREMENT=37689 DEFAULT CHARSET=utf8;


CREATE TABLE `usericon` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) unsigned DEFAULT NULL,
  `icon_group` int(11) DEFAULT NULL,
  `icon_gname` varchar(20) DEFAULT NULL,
  `icon_name` varchar(20) DEFAULT NULL,
  `icon_filename` varchar(80) DEFAULT NULL,
  `icon_width` int(11) DEFAULT NULL,
  `icon_height` int(11) DEFAULT NULL,
  `color` varchar(7) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `usericon_user_id` (`user_id`)
) ENGINE=InnoDB AUTO_INCREMENT=403 DEFAULT CHARSET=utf8;


CREATE TABLE `userlogin` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) unsigned DEFAULT NULL,
  `uname` varchar(20) DEFAULT NULL,
  `login_type` bigint(20) DEFAULT NULL,
  `created_ip` varchar(20) DEFAULT NULL,
  `updatedat` datetime DEFAULT NULL,
  `createdat` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `userlogin_user_id` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


-- 2021-01-31 14:53:16
