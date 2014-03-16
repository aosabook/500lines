var fs = require('fs');
var passwordHash = require('password-hash');
var tools = require('./tools.js');
var path = require('path');

function FileStore(storeDir, usersDir) {
  this.fileStoreDir = storeDir;
  this.fileStoreUsersDir = usersDir;
}

module.exports = function(storeDir, usersDir, callback) {
  var store = new FileStore(storeDir, usersDir);
  store.setup(callback);
};

FileStore.prototype.createFolder = function(folder){
  try{
    fs.statSync(folder);
  }catch(statErr){
    if(statErr.code !== 'ENOENT') throw statErr;
    fs.mkdirSync(folder);
  }
};

FileStore.prototype.setup = function(callback) {
  this.createFolder(this.fileStoreDir);
  this.createFolder(this.fileStoreUsersDir);
  callback(null, this);
};

FileStore.prototype.getWikiContents = function(page, callback){
  return fs.readFile(path.join(this.fileStoreDir, page), "utf-8", function(error, data){
    if (error && error.code !== 'ENOENT') return callback(error);
    if(!data) data = '{}'; //empty doc
    tools.parseJSON(data, function(err, parsedData){
      if(err) return callback(err);
      return callback(null, parsedData);
    });
  });
};

FileStore.prototype.saveWikiContents = function(args, callback){
  args.updatedDate = new Date();
  if(!args._rev){
    args._rev = 1;
    return this.saveWikiPage(args, callback);
  }
  return this.updateWikiPageIfNoConflict(args, callback);
};

FileStore.prototype.updateWikiPageIfNoConflict = function(args, callback){
  this.getWikiContents(args._id, function(error, doc){
    if(error) return callback(error);
    var newRev = parseInt(args._rev);
    var savedRev = doc._rev;
    if(isNaN(newRev)) return callback(new Error('Invalid revision: '+args._rev));
    if(savedRev != args._rev) return callback(new Error('conflict'));
    newRev++;
    return this.saveWikiPage(args, callback);
  }.bind(this));
};


FileStore.prototype.saveWikiPage = function(args, callback){
  fs.writeFile(path.join(this.fileStoreDir,args._id), JSON.stringify(args), function(error){
    if(error) return callback(error);
    return callback(null, 'ok');
  });
};

FileStore.prototype.listWikiPages = function(callback){
  fs.readdir(this.fileStoreDir, function(err, files){
    if(err) return callback(err);
    var rows = Object.keys(files).map(function(i) {return {id: files[i]};});
    return callback(null, rows);
  });
};

FileStore.prototype.insertUser = function(username, password, email, callback){
  var filePath = path.join(this.fileStoreUsersDir, username);
  //check for existing user first
  this.getUser(username, function(error, user){
    if(error && error.message !== 'User does not exist') return callback(error);
    if(user) return callback(new Error('Username already exists'));
    var args = {_id: username, name: username, type: "user", roles: [], password: passwordHash.generate(password), email: email };
    fs.writeFile(filePath, JSON.stringify(args), function(error){
      if(error) return callback(error);
      return callback(null, username);
    });
  });
};

FileStore.prototype.authenticate = function(username, password, callback){
  var args = {name: username, password: password};
  this.getUser(username, function(error, data){
    if(error) return callback(error);
    if(!data) return callback(new Error("User not found"));
    if(!passwordHash.verify(password, data.password)) return callback(new Error("Invalid Password"));
    return callback(null, data); //ok
  });
};

FileStore.prototype.getUser = function(username, callback){
  if(!username) return callback(new Error("No username provided."));
  fs.readFile(path.join(this.fileStoreUsersDir, username), 'utf-8', function(error, data){
    if(error){
      if(error.code == 'ENOENT') return callback(new Error('User does not exist'));
      return callback(error);
    }
    if(!data) return callback(new Error('User does not exist'));
    tools.parseJSON(data, function(err, parsedData){
      if(err) return callback(err);
      return callback(null, parsedData);
    });
  });
};
