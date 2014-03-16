exports.configureStore = function(callback){
  var fileStoreDir = './files/'; //relative to where npm start is launched
  var fileStoreUsersDir = './users/';
  require("../file_store.js")(fileStoreDir, fileStoreUsersDir, callback);
};
