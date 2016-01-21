# FDB

CircleDB is an in-memory, no-sql functional database, written in Clojure.

It is a modest attempt to provide part of the functionality that the Datomic database provides (the main omitted functionality is the durability part).

The core idea is to add the notion of time to the data, and provide on top of the standard CRUD operations, a set of time related operations.

The database itself is a collection of datums. A datum is built of an entity (think of a row in a table) that has its attributes (a column) and the value of that entity's attribute at a given time.
Any update does not overwrite the previous value, but addes another datum to the database.

The database supports both DB "modifying" (modifying as in creating a new DB value) transactions and what-if actions.

