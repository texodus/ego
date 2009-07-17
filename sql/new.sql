SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;
SET search_path = public, pg_catalog;
SET default_tablespace = '';
SET default_with_oids = false;

--------------------------------------------------------------------------------
--
-- Accounts

CREATE SEQUENCE accounts_primary_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE accounts (
    id bigint DEFAULT nextval(('accounts_primary_seq'::text)::regclass) NOT NULL,
    username text NOT NULL,
    password text NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    registration_timestamp timestamp without time zone DEFAULT 'now' NOT NULL,
    last_login_timestamp timestamp without time zone DEFAULT 'now' NOT NULL,
    CONSTRAINT id_primary PRIMARY KEY (id)
);

CREATE INDEX accounts_id_idx ON accounts USING btree (id);

--------------------------------------------------------------------------------
--
-- Friends

CREATE TABLE friends (
    account_id bigint NOT NULL,
    friend_id bigint NOT NULL,
    registration_date timestamp without time zone DEFAULT 'now' NOT NULL,
    CONSTRAINT friends_primary_key PRIMARY KEY (account_id, friend_id),
    CONSTRAINT friends_first_id_foreign FOREIGN KEY (account_id) REFERENCES accounts(id),
    CONSTRAINT friends_second_id_foreign FOREIGN KEY (friend_id) REFERENCES accounts(id)
);

CREATE INDEX friends_first_id_idx ON friends USING btree (account_id);
CREATE INDEX friends_second_id_idx ON friends USING btree (friend_id);

--------------------------------------------------------------------------------
--
-- Messages

CREATE SEQUENCE messages_primary_seq
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE messages (
    id bigint DEFAULT nextval(('messages_primary_seq'::text)::regclass) NOT NULL,
    sender_id bigint NOT NULL,
    recip_id bigint NOT NULL,
    message text NOT NULL,
    sent_timestamp timestamp without time zone DEFAULT 'now' NOT NULL,
    CONSTRAINT messages_primary_key PRIMARY KEY (id),
    CONSTRAINT messages_sender_id_foreign FOREIGN KEY (sender_id) REFERENCES accounts(id),
    CONSTRAINT messages_recip_id_foreign FOREIGN KEY (recip_id) REFERENCES accounts(id)
);

CREATE INDEX messages_sender_id_idx ON messages USING btree (sender_id);
CREATE INDEX messages_recip_id_idx ON messages USING btree (recip_id);

--------------------------------------------------------------------------------
--
-- Teardown

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;

