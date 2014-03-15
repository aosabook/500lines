var fs = require('fs');

module.exports = function configureFileStore(){
  var store = require('../file_store.js');
  store.fileStoreDir = './files/'; //relative to where npm start is launched
  store.fileStoreUsersDir = './users/';

  //check if folders exist and create if not
  fs.stat(store.fileStoreDir, function(error, stats){
    if(error) fs.mkdir(store.fileStoreDir);
  });
  fs.stat(store.fileStoreUsersDir, function(error, stats){
    if(error) fs.mkdir(store.fileStoreUsersDir);
  });
  console.log('Using file store at ' + store.fileStoreDir);
  return store;
};
