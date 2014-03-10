# 500 Lines or Less Wiki #

This is a basic wiki built using node.js with a configurable backend of CouchDB or a local file store, and passport authentication with the same local user store by username and password.

## Setup ##

Install Node.js
http://nodejs.org/download/
I'm using current stable v0.10.26.

Install CouchDB (to use optional CouchDB store)
http://docs.couchdb.org/en/latest/install/index.html
I'm using current stable v.1.4.0.
Start couchdb.
Create a new db called wiki:
  curl -X PUT http://127.0.0.1:5984/wiki -> {"ok":true}

Configure the wiki in config.js, specifying either config_file.js or config_db.js for the store, using config_*.js to provide appropriate config for each.

Run npm install in the js directory to get the required library dependencies in your local workspace.

Run the server with npm start.  Visit the wiki at http://localhost:8080/wiki/. Use the signup button to create a new user and log in.
