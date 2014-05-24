// nodeunit test suite to ensure that the environment is set up correctly

var config = require('../app/config.js');
var wiki = require('../app/server.js');
var request = require('request');

exports.checkDatabaseRunning = function(test){
  console.log('Connecting to database at: ' + wiki.dbStore.dbURL);
  request(wiki.dbStore.dbURL, function(error, couchResponse, doc){
    test.ifError(error);
    test.equal(couchResponse.statusCode, 200, 'Invalid response from CouchDB, check installation and config');
    test.done();
  });
};

exports.checkWikiDatabaseExists = function(test){
  console.log('Connecting to wiki database at: ' + wiki.dbStore.dbURL + wiki.dbStore.dbName);
  request(wiki.dbStore.dbURL + wiki.dbStore.dbName, function(error, couchResponse, doc){
    test.ifError(error);
    test.equal(couchResponse.statusCode, 200, 'Invalid response from CouchDB, ensure db ' + wiki.dbStore.dbName + ' has been created.');
    test.done();
  });
};

exports.checkWebServerRunning = function(test){
  request('http://localhost:'+config.webserverport+'/wiki', function(error, response, content){
    test.ifError(error);
    test.equal(response.statusCode, 200, 'Web server not started successfully. Check console for errors.');
    test.done();
  });
};
