// nodeunit test suite to ensure that the environment is set up correctly

var config = require('../app/config.js');
var request = require('request');

exports.checkDatabaseRunning = function(test){
  console.log('Connecting to database at: ' + config.dbURL);
  request(config.dbURL, function(error, couchResponse, doc){
    test.ifError(error);
    test.equal(couchResponse.statusCode, 200, 'Invalid response from CouchDB, check installation and config');
    test.done();
  });
};

exports.checkWikiDatabaseExists = function(test){
  console.log('Connecting to wiki database at: ' + config.dbURL + config.dbName);
  request(config.dbURL + config.dbName, function(error, couchResponse, doc){
    test.ifError(error);
    test.equal(couchResponse.statusCode, 200, 'Invalid response from CouchDB, ensure db ' + config.dbURL + config.dbName + ' has been created.');
    test.done();
  });
};

exports.checkWebServerRunning = function(test){
  var wiki = require('../app/server.js');
  request('http://localhost:'+config.webserverport+'/wiki', function(error, response, content){
    test.ifError(error);
    test.equal(response.statusCode, 200, 'Web server not started successfully. Check console for errors.');
    wiki.shutDown();
    test.done();
  });
};
