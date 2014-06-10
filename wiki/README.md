# 500 Lines or Less Wiki #

This is a basic wiki built using node.js with a CouchDB persistence layer, local passport authentication, and client-side jQuery.

## Setup ##

### Install Node.js ###
* See [node.js download site](http://nodejs.org/download/)
* I'm using current stable v0.10.26.

### Install CouchDB ###
* [CouchDB install docs](http://docs.couchdb.org/en/latest/install/index.html)
* I'm using current stable v.1.4.0.
* Start CouchDB.

### Configure Wiki ###
* Configure the wiki in app/config.js, specifying your database URL and ports as needed.

### Install ###
* Run npm install to get the required library dependencies in your local workspace.

### Init DB ###
* Once the database is installed and running, and URLs have been configured in app/config.js, run "npm run initdb" to create a new empty database to store the wiki documents.

### Run tests ###
* Run npm test to execute the tests and check that the system is behaving as expected.

### Run Wiki ###
* Run the server with npm start.
* Visit the wiki at [http://localhost:8080/wiki/](http://localhost:8080/wiki/).
* Use the signup button to create a new user and log in.
* Use the add button to launch the editor for a new wiki page.
