const config = require('./config.js'),
      util = require('./util.js'),
      request = require('request');

module.exports = {
  getWikiContents: function(page, toHtml, callback){
    request(config.couchDBURL + config.couchDBName + page, function(error, couchResponse, doc){
      if(error) return response.send(couchResponse.statusCode, error);
      doc = JSON.parse(doc);
      if(doc.error && doc.reason == "missing"){
        doc.content = ''; //page does not exist yet
      }
      if(toHtml) doc.content = util.processHtml(doc.content);
      callback(doc);
    });
  },
  saveWikiContents: function(page, contents, revision, user, comment, callback){
    var args = {_id: page, content: contents, comment: comment, updatedDate: new Date(), user: user.name};
    if(revision) args['_rev'] = revision; //if a revision exists, it must be supplied to update, otherwise leave blank to insert
    request({url: config.couchDBURL + config.couchDBName + page, method: 'PUT', json: args}, function(error, couchResponse, content){
        if(error) return callback(error);
        if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback('ok'); //ok or created or not changed
        if(couchResponse.statusCode == 409) return callback('conflict');
        if(content.error) return callback(content.error);
        return callback('error'); //just in case there's some other error scenario that doesn't provide an error message
    });
  },
  listWikiPages: function(callback){
    request(config.couchDBURL + config.couchDBName + '_all_docs', function(error, couchResponse, content){
      if(error){
        console.log('Error retreiving pages from couchDB at ' + config.couchDBURL + ": " +error);
        if(couchResponse) return callback(couchResponse.statusCode, error, null);
        else return callback(500, 'Server error, unable to display wiki contents.', null);
      }
      return callback(200, null, content);
    });
  },
  getId: function(username){
    return "org.couchdb.user:"+username;
  },
  insertUser: function(username, password, callback){
    var userId = this.getId(username);
    var args = {"_id": userId, "name": username, "type": "user", "roles": [], "password": password };
    request({url: config.couchDBURL + '_users/'+userId, method: 'PUT', json: args}, function(error, couchResponse, content){
      if(error) return callback(error);
      if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback('ok'); //ok or created or not changed
      if(couchResponse.statusCode == 409) return callback('User already exists');
      if(content.error) return callback(content.error);
      return callback('Error ' + couchResponse.statusCode); //just in case there's some other error scenario that doesn't provide an error message
    })
  },
  authenticate: function(username, password, callback){
    //create session
    var args = {"name": username, "password": password};
    request({url: config.couchDBURL + '_session', method: 'POST', json: args}, function(error, couchResponse, content){
      if(error) return callback(error);
      if(couchResponse.statusCode == 200) return callback(null, content);
      if(content.error) return callback(content.reason);
      return callback('Error ' + couchResponse.statusCode);
    });
  },
  getUser: function(username, callback){
    var userid = this.getId(username);
    request(config.couchDBURL + '_users/'+userid, function(error, couchResponse, userDoc){
      if(error) callback(error);
      userDoc = JSON.parse(userDoc);
      if(userDoc.error) callback(userDoc.error);
      callback(null, userDoc);
    });
  }
}

