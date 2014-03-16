exports.configureStore = function(callback){
  var dbURL = 'http://localhost:5984/';
  var dbName = 'wiki/';
  require('../db_store.js')(dbURL, dbName, callback);
};
