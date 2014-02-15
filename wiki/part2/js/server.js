var express = require('express'),
    requestMod = require('request'),
    fs = require('fs'),
    whiskers = require('whiskers'),
    marked = require('marked'),
    app = express();

var couchDBURL = 'http://localhost:5984/wiki/';

app.use(express.bodyParser()); //for post parameter parsing
app.engine('.html', whiskers.__express);
app.set('views', __dirname+'/../views');

processHtml = function(wikiMarkup){
  return marked(wikiMarkup);
}

getWikiContents = function(page, toHtml, callback){
  requestMod(couchDBURL + page, function(error, couchResponse, doc){
    if(error) return response.send(couchResponse.statusCode, error);
    doc = JSON.parse(doc);
    if(doc.error && doc.reason == "missing"){
      doc.content = ''; //page does not exist yet
    }
    if(toHtml) doc.content = processHtml(doc.content);
    callback(doc);
  });
}

saveWikiContents = function(page, contents, revision, comment, callback){
  var args = {_id: page, content: contents, comment: comment};
  if(revision) args['_rev'] = revision; //if a revision exists, it must be supplied to update, otherwise leave blank to insert
  requestMod({url: couchDBURL + page, method: 'PUT', json: args}, function(error, couchResponse, content){
      if(error) return callback(error);
      if(couchResponse.statusCode == 200 || couchResponse.statusCode == 201 || couchResponse.statusCode == 304) return callback('ok'); //ok or created or not changed
      if(couchResponse.statusCode == 409) return callback('conflict');
      if(content.error) return callback(content.error);
      return callback('error'); //just in case there's some other error scenario that doesn't provide an error message
  });
}

getSeparator = function(path){
  //if path already ends in /, return empty string, otherwise return /
  return /\/$/.test(path) ? '': '/';
}

app.get('/wiki/', function(request, response){
  requestMod(couchDBURL + '_all_docs', function(error, couchResponse, content){
    if(error) return response.send(couchResponse.statusCode, error);
    var addLink = request.path + getSeparator(request.path) + "add";
    response.render('list.html', {pages: JSON.parse(content).rows, url: addLink});
  });
});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  //read page if it exists
  var content = getWikiContents(page, true, function(doc){
    var editLink = request.path + getSeparator(request.path) + "edit";
    //show contents of page
    response.render('view.html', {title: page, content: doc.content, editLink: editLink} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var url = request.path;
  var content = getWikiContents(page, false, function(doc){
    response.render('edit.html', {title: page, content: doc.content, revision: doc._rev, url: url} );
  });
});

app.post('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var revision = request.body.revision;
  var content = request.body.content; //TODO: sanitize input
  var comment = request.body.comment;
  saveWikiContents(page, content, revision, comment, function(status){
    if(status === 'ok') response.redirect('/wiki/'+page); //return to view
    else{ //display contents in editor
      var error = 'Unable to save contents: ' + status;
      if(status === "conflict"){
        var error = 'Unable to save contents due to a conflicting update.  Your version is shown above.  Refresh to load the latest version.';
      }
      response.render('edit.html', {title: page, content: content, revision: revision, url: request.path, error: error, comment: comment} );
    }
  });
});

app.post('/wiki/add', function(request, response){
  var page = request.body.title;
  response.redirect('/wiki/'+page+'/edit');
});

app.listen(8080, function(){
  console.log('listening...');
});
