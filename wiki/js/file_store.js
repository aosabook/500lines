var wiki_util = require('./wiki_util.js');
var fs = require('fs');
var passwordHash = require('password-hash');

module.exports = {
  getWikiContents: function(page, toHtml, callback){
    return fs.readFile(this.fileStoreDir+page, "utf-8", function(err, data){
      if(data) data = JSON.parse(data);
      else data = {}; //empty doc
      if(toHtml && data.content) data.content = wiki_util.processHtml(data.content);
      callback(data);
    });
  },
  saveWikiContents: function(args, callback){
    args.updatedDate = new Date();
    if(args._rev){
      return this.updateWikiPageIfNoConflict(args, callback);
    }else{
      args._rev = 1;
      return this.saveWikiPage(args, callback);
    }
  },
  updateWikiPageIfNoConflict: function(args, callback){
    var context = this;
    this.getWikiContents(args._id, false, function(doc){
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
      if(err){
        console.log(err);
        return callback(new Error('Server error, unable to display wiki contents.'), null);
      }
      var rows = Object.keys(files).map(function(i) {return {id: files[i]};});
      return callback(null, JSON.stringify({rows: rows}));
    });
  },
  insertUser: function(username, password, callback){
    var path = this.fileStoreUsersDir+username;
    //check for existing user first
    this.getUser(username, function(error, user){
      if(user) return callback(new Error("Username already exists"));
      //write new user record
      var args = {_id: username, name: username, type: "user", roles: [], password: passwordHash.generate(password) };
      fs.writeFile(path, JSON.stringify(args), function(error){
        if(error) return callback(error);
        return callback(null);
      });
    });
  },
  authenticate: function(username, password, callback){
    var args = {name: username, password: password};
    //get user from disk
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
      if(data) callback(null, JSON.parse(data));
      else callback(null, null); //user does not exist, return back null user
    });
  }
};
