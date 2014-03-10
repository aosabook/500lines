var wiki_util = require('./wiki_util.js');
var request = require('request');

module.exports = {
  getWikiContents: function(page, toHtml, callback){
    request(this.dbURL + this.dbName + page, function(error, couchResponse, doc){
      if(error) return response.send(couchResponse.statusCode, error);
      doc = JSON.parse(doc);
      if(doc.error && doc.reason == "missing"){
        doc.content = ''; //page does not exist yet
      }
      if(toHtml) doc.content = wiki_util.processHtml(doc.content);
      callback(doc);
    });
  },
  saveWikiContents: function(args, callback){
    args.updatedDate = new Date();
    request({url: this.dbURL + this.dbName + page, method: 'PUT', json: args}, function(error, couchResponse, content){
        if(error) return callback(error);
        if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback('ok'); //ok or created or not changed
        if(couchResponse.statusCode == 409) return callback(new Error('conflict'));
        if(content.error) return callback(content.error);
        return callback(new Error('error'); //just in case there's some other error scenario that doesn't provide an error message
    });
  },
  listWikiPages: function(callback){
    request(this.dbURL + this.dbName + '_all_docs', function(error, couchResponse, content){
      if(error){
        console.log('Error retreiving pages from couchDB at ' + this.dbURL + ": " +error);
        return callback(error, null);
      }
      return callback(null, content);
    });
  },
  getId: function(username){
    return "org.couchdb.user:"+username;
  },
  insertUser: function(username, password, callback){
    var userId = this.getId(username);
    var args = {_id: userId, name: username, type: "user", roles: [], password: password };
    request({url: this.dbURL + '_users/'+userId, method: 'PUT', json: args}, function(error, couchResponse, content){
      if(error) return callback(error);
      if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback(null); //ok or created or not changed
      if(couchResponse.statusCode == 409) return callback('User already exists');
      if(content.error) return callback(content.error);
      return callback(new Error('Error ' + couchResponse.statusCode)); //just in case there's some other error scenario that doesn't provide an error message
    });
  },
  authenticate: function(username, password, callback){
    //create session
    var args = {name: username, password: password};
    request({url: this.dbURL + '_session', method: 'POST', json: args}, function(error, couchResponse, content){
      if(error) return callback(error);
      if(couchResponse.statusCode == 200) return callback(null, content);
      if(content.error) return callback(new Error(content.reason));
      return callback(new Error('Error ' + couchResponse.statusCode));
    });
  },
  getUser: function(username, callback){
    var userid = this.getId(username);
    request(this.dbURL + '_users/'+userid, function(error, couchResponse, userDoc){
      if(error) callback(error);
      userDoc = JSON.parse(userDoc);
      if(userDoc.error) callback(userDoc.error);
      callback(null, userDoc);
    });
  }
};
