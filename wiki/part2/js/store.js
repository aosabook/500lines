const config = require('./config.js'),
      util = require('./util.js'),
      requestMod = require('request');

module.exports = {
  getWikiContents: function(page, toHtml, callback){
    requestMod(config.couchDBURL + page, function(error, couchResponse, doc){
      if(error) return response.send(couchResponse.statusCode, error);
      doc = JSON.parse(doc);
      if(doc.error && doc.reason == "missing"){
        doc.content = ''; //page does not exist yet
      }
      if(toHtml) doc.content = util.processHtml(doc.content);
      callback(doc);
    });
  },
  saveWikiContents: function(page, contents, revision, comment, callback){
    var args = {_id: page, content: contents, comment: comment, updatedDate: new Date()};
    if(revision) args['_rev'] = revision; //if a revision exists, it must be supplied to update, otherwise leave blank to insert
    requestMod({url: config.couchDBURL + page, method: 'PUT', json: args}, function(error, couchResponse, content){
        if(error) return callback(error);
        if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback('ok'); //ok or created or not changed
        if(couchResponse.statusCode == 409) return callback('conflict');
        if(content.error) return callback(content.error);
        return callback('error'); //just in case there's some other error scenario that doesn't provide an error message
    });
  },
  listWikiPages: function(callback){
    requestMod(config.couchDBURL + '_all_docs', function(error, couchResponse, content){
      if(error){
        console.log('Error retreiving pages from couchDB at ' + config.couchDBURL + ": " +error);
        if(couchResponse) return callback(couchResponse.statusCode, error, null);
        else return callback(500, 'Server error, unable to display wiki contents.', null);
      }
      return callback(200, null, content);
    });
  }
}

