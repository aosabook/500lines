module.exports = function configureDBStore(){
  var store = require('./db_store.js');
  store.dbURL = 'http://localhost:5984/';
  store.dbName = 'wiki/';
  return store;
};
