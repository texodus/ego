--------------------------------------------------------------------------------
--
-- Test Data

INSERT INTO accounts (username, password, first_name, last_name) VALUES ('alice', md5('password'), 'Alice', 'Smith');
INSERT INTO accounts (username, password, first_name, last_name) VALUES ('bob', md5('password'), 'Bob', 'Smith');
INSERT INTO accounts (username, password, first_name, last_name) VALUES ('joe', md5('password'), 'Joe', 'Smith');
INSERT INTO accounts (username, password, first_name, last_name) VALUES ('dan', md5('password'), 'Dan', 'Smith');
INSERT INTO accounts (username, password, first_name, last_name) VALUES ('andrew', md5('password'), 'Andrew', 'Stein');

INSERT INTO friends (account_id, friend_id) 
       VALUES ((SELECT id FROM accounts WHERE username LIKE 'andrew'), 
       	       (SELECT id FROM accounts WHERE username LIKE 'bob'))
