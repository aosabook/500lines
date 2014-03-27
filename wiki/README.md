# 500 Lines or Less Wiki #

This is a basic wiki built using node.js with a CouchDB persistence layer, local passport authentication, and client-side jQuery components.

## Setup ##

### Install Node.js ###
* [node.js download site](http://nodejs.org/download/)
* I'm using current stable v0.10.26.

### Install CouchDB ###
* [CouchDB docs](http://docs.couchdb.org/en/latest/install/index.html)
* I'm using current stable v.1.4.0.
* Start couchdb.
* Create a new db called wiki:
  curl -X PUT http://127.0.0.1:5984/wiki -> {"ok":true}

### Configure Wiki ###
* Configure the wiki in app/config.js, specifying your database URL and ports as needed.

### NPM Install ###
* Run npm install to get the required library dependencies in your local workspace.

### Run Wiki ###
* Run the server with npm start.
* Visit the wiki at [http://localhost:8080/wiki/](http://localhost:8080/wiki/).
* Use the signup button to create a new user and log in.
* Use the add button to launch the editor for a new wiki page.
