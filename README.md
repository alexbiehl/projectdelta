Try on the task given on https://github.com/projectdeltasea/projectdelta. This is just an experiment to see if I can do it with my .

So far there are:
  - business logic, all pure
  - api exposed through simple monad
  - persistence using acid-state, guarantees ACID transactions
  - scotty as RESTful application server

The task is copied from https://github.com/projectdeltasea/projectdelta.

##Task

Write a program for authenticating employees of a company for access to various internal tools. The company has decided that:
- users are identified by an email address;
- each user belongs to 1 or more groups;
- each group is granted certain privileges.

For example, user `haskell.curry@projectdelta.com` belongs to `math` group. Group `math` has the following permissions:

- can read and send emails to the `math@projectdelta.com` mailing list
- can reserve conference rooms via `https://scheduling.office.projectdelta.com/`
- has read-only access to financial reports at `https://reports.finance.projectdelta.com/

Administrators should be able to:

- create new users and groups
- grant permissions to a group
- list the permissions a user has
...and anything else you see fit to implement

The design and implementation is up to you, including the structure of the data and the interface you expose. For example, you could write a commandline tool, a TCP daemon, or a RESTful web service. The only other requirements are that the permissions must be persistent and it must be written in Haskell.
