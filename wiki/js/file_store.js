var fs = require('fs');
var passwordHash = require('password-hash');
var tools = require('./tools.js');

module.exports = {
  getWikiContents: function(page, callback){
    return fs.readFile(this.fileStoreDir+page, "utf-8", function(error, data){
      if (error && error.code !== 'ENOENT') return callback(error);
      if(!data) data = '{}'; //empty doc
      tools.parseJSON(data, function(err, parsedData){
        if(err) return callback(err);
        return callback(null, parsedData);
      });
    });
  },
  saveWikiContents: function(args, callback){
    args.updatedDate = new Date();
    if(!args._rev){
      args._rev = 1;
      return this.saveWikiPage(args, callback);
    }
    return this.updateWikiPageIfNoConflict(args, callback);
  },
  updateWikiPageIfNoConflict: function(args, callback){
    var context = this;
    this.getWikiContents(args._id, function(error, doc){
      if(error) return callback(error);
      var savedRev = doc._rev;
      if(savedRev != args._rev) return callback(new Error('conflict'));
      args._rev = parseInt(args._rev)+1;
      return context.saveWikiPage(args, callback);
    });
  },
  saveWikiPage: function(args, callback){
    fs.writeFile(this.fileStoreDir+args._id, JSON.stringify(args), function(error){
      if(error) return callback(error);
      return callback(null, 'ok');
    });
  },
  listWikiPages: function(callback){
    fs.readdir(this.fileStoreDir, function(err, files){
      if(err) return callback(new Error('Server error, unable to display wiki contents.'));
      var rows = Object.keys(files).map(function(i) {return {id: files[i]};});
      return callback(null, rows);
    });
  },
  insertUser: function(username, password, callback){
    var path = this.fileStoreUsersDir+username;
    //check for existing user first
    this.getUser(username, function(error, user){
      if(error && error.message !== 'User does not exist') return callback(error);
      if(user) return callback(new Error('conflict'));
      var args = {_id: username, name: username, type: "user", roles: [], password: passwordHash.generate(password) };
      fs.writeFile(path, JSON.stringify(args), function(error){
        if(error) return callback(error);
        return callback(null, username);
      });
    });
  },
  authenticate: function(username, password, callback){
    var args = {name: username, password: password};
    this.getUser(username, function(error, data){
      if(error) return callback(error);
      if(!data) return callback(new Error("User not found"));
      if(!passwordHash.verify(password, data.password)) return callback(new Error("Invalid Password"));
      return callback(null, data); //ok
    });
  },
  getUser: function(username, callback){
    fs.readFile(this.fileStoreUsersDir + username, "utf-8", function(error, data){
      if(error) return callback(error);
      if(!data) return callback(new Error('User does not exist'));
      tools.parseJSON(data, function(err, parsedData){
        if(err) return callback(err);
        return callback(null, parsedData);
      });
    });
  }
};
