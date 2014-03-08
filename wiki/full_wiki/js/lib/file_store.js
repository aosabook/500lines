const config = require('../config.js'),
      util = require('./util.js'),
      fs = require('fs'),
      passwordHash = require('password-hash');

module.exports = {
  getWikiContents: function(page, toHtml, callback){
    return fs.readFile(config.fileStoreDir+page, "utf-8", function(err, data){
      if(data) data = JSON.parse(data);
      else data = {}; //empty doc
      if(toHtml && data.content) data.content = util.processHtml(data.content);
      callback(data);
    });
  },
  saveWikiContents: function(page, contents, revision, user, comment, callback){
    var args = {_id: page, content: contents, comment: comment, updatedDate: new Date(), user: user.name};
    if(revision){
      //update to existing doc, check no conflict before update
      //get file on disk to check for conflict before update
      this.getWikiContents(page, false, function(doc){
        var savedRev = doc._rev;
        if(savedRev != revision) return callback('conflict');
        args['_rev'] = parseInt(revision)+1;
        fs.writeFile(config.fileStoreDir+page, JSON.stringify(args), function(error){
          if(error) return callback(error);
          return callback('ok');
        });
      });
    }
    else{
      //new page, just insert with revision 1
      args['_rev'] = 1;
      fs.writeFile(config.fileStoreDir+page, JSON.stringify(args), function(error){
        if(error) return callback(error);
        return callback('ok');
      });
    }
  },
  listWikiPages: function(callback){
    fs.readdir(config.fileStoreDir, function(err, files){
      if(err) return callback(500, 'Server error, unable to display wiki contents.', null);
      var rows = [];
      for(var i in files){
        rows.push({id: files[i]});
      }
      return callback(200, null, JSON.stringify({rows: rows}));
    });
  },
  insertUser: function(username, password, callback){
    var userId = username;
    //check for existing user first
    this.getUser(username, function(error, user){
      if(user != null) return callback("Username already exists");
      //write new user record
      var args = {"_id": userId, "name": username, "type": "user", "roles": [], "password": passwordHash.generate(password) };
      fs.writeFile(config.fileStoreUsersDir+username, JSON.stringify(args), function(error){
        if(error) return callback(error);
        return callback('ok');
      });
    });
  },
  authenticate: function(username, password, callback){
    var args = {"name": username, "password": password};
    //get user from disk
    this.getUser(username, function(error, data){
      if(error) return callback(error);
      if(!data) return callback("User not found");
      if(passwordHash.verify(password, data.password)) return callback(null, data);
      else return callback("Invalid Password");
    });
  },
  getUser: function(username, callback){
    fs.readFile(config.fileStoreUsersDir + username, "utf-8", function(error, data){
      if(error) return callback(error);
      if(data){
        data = JSON.parse(data);
        callback(null, data);
      }else{
        callback(null, null); //user does not exist, return back null user
      }
    });
  }
}

