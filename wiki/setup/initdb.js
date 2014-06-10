var config = require('../app/config.js');
var request = require('request');

var dbURL = config.dbURL + config.dbName;

request({url: dbURL, method: 'PUT'}, function(error, couchResponse, content){
  if(error) return console.error("Unable to initialize database. Please check that CouchDB is installed and running at " + config.dbURL + ". " + error);
  if(couchResponse.statusCode == 412) return console.log('Database already initialized at ' + dbURL + ". Run npm start to start the wiki app.");
  if(couchResponse.statusCode != 201) return console.error("Received error initializing wiki database." + couchResponse.statusCode + content);
  console.log('Database initialized successfully at ' + dbURL + ". Run npm start to start the wiki app.");
});

