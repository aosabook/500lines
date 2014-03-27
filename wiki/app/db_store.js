var request = require('request');
var url = require('url');

function DBStore(dbURL, dbName){
  this.dbURL = dbURL;
  this.dbName = dbName;
}

module.exports = function(dbURL, dbName){
  return new DBStore(dbURL, dbName);
};

DBStore.prototype.parseJSON = function(data, callback){
  try{
    return callback(null, JSON.parse(data));
  }catch(e){
    return callback(e, null);
  }
};

DBStore.prototype.handleCouchResponse = function(error, couchResponse, content, callback){
    if(error) return callback(error);
    if(couchResponse.statusCode == 409) return callback(new Error('conflict'));
    if(couchResponse.statusCode != 200 && couchResponse.statusCode != 201 && couchResponse.statusCode != 304) return callback(new Error("Bad status code: " + couchResponse.statusCode)); //not ok or created or not changed
    return callback(null, content);
};

DBStore.prototype.getWikiContents = function(page, callback){
  request(this.dbURL + this.dbName + page, function(error, couchResponse, doc){
    if(error) return callback(error);
    this.parseJSON(doc, function(err, object){
      if(err) return callback(err);
      if(object.err && object.reason !== 'missing') return callback(new Error(object.err + ": " + object.reason));
      if(!object.content) object.content = ''; //page does not exist yet
      return callback(null, object);
    });
  }.bind(this));
};

DBStore.prototype.saveWikiContents = function(args, callback){
  request({url: this.dbURL + this.dbName + args._id, method: 'PUT', json: args}, function(error, couchResponse, content){
    this.handleCouchResponse(error, couchResponse, content, callback);
  }.bind(this));
};

DBStore.prototype.listWikiPages = function(callback){
  request(this.dbURL + this.dbName + '_all_docs', function(error, couchResponse, content){
    if(error) return callback(error, null);
    this.parseJSON(content, function(err, object){
      if(err) return callback(err, null);
      return callback(null, object.rows);
    });
  }.bind(this));
};

DBStore.prototype.getId = function(username){
  return "org.couchdb.user:"+username;
};

DBStore.prototype.insertUser = function(username, password, email, callback){
  var userId = this.getId(username);
  var args = {_id: userId, name: username, type: "user", roles: [], password: password, email: email};
  request({url: this.dbURL + '_users/'+userId, method: 'PUT', json: args}, function(error, couchResponse, content){
    this.handleCouchResponse(error, couchResponse, content, callback);
  }.bind(this));
};

DBStore.prototype.authenticate = function(username, password, callback){
  var args = {name: username, password: password};
  request({url: this.dbURL + '_session', method: 'POST', json: args}, function(error, couchResponse, content){
    this.handleCouchResponse(error, couchResponse, content, callback);
  }.bind(this));
};

DBStore.prototype.getUser = function(username, callback){
  var userid = this.getId(username);
  request(this.dbURL + '_users/'+userid, function(error, couchResponse, userDoc){
    if(error) callback(error);
    this.parseJSON(userDoc, function(err, user){
      if(err) return callback(err);
      if(user.error) return callback(user.error);
      return callback(null, user);
    });
  }.bind(this));
};
