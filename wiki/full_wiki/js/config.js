module.exports = {
  useDBStore: false, //if true, also specify dbURL and dbName for a CouchDB instance.  if false, also specify fileStoreDir and fileStoreUsersDir for local storage.
  dbURL: 'http://localhost:5984/',
  dbName: 'wiki/',
  fileStoreDir: '../files/',
  fileStoreUsersDir: '../users/',
  webserverport: 8080,
  viewsdir: __dirname+'/../views'
}
