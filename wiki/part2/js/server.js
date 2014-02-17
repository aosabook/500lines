var express = require('express'),
    requestMod = require('request'),
    fs = require('fs'),
    whiskers = require('whiskers'),
    marked = require('marked'),
    dateformat = require('dateformat'),
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
  var args = {_id: page, content: contents, comment: comment, updatedDate: new Date()};
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

showCompareEditor = function(request, response, args){
  //get latest version of doc from couch
  getWikiContents(args.title, false, function(doc){
    //show contents of both pages
    args['comparecontent'] = doc.content;
    args['comparecomment'] = doc.comment;
    args['comparedate'] = dateformat(doc.updatedDate, "h:MMTT d-mmm-yyyy");
    args['revision'] = doc._rev;
    response.render('layout.html', args);
  });
}

app.get('/wiki', function(request, response){
  requestMod(couchDBURL + '_all_docs', function(error, couchResponse, content){
    if(error){
      if(couchResponse) return response.send(couchResponse.statusCode, error);
      else{
        console.log(error);
        return response.send(500, 'Server error, unable to display wiki contents');
      }
    }
    var addLink = request.path + getSeparator(request.path) + "add";
    response.render('layout.html', {pages: JSON.parse(content).rows, url: addLink,
                                   partials: {body: 'list.html'}});
  });
});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  //read page if it exists
  getWikiContents(page, true, function(doc){
    var editLink = request.path + getSeparator(request.path) + "edit";
    //show contents of page
    response.render('layout.html', {title: page, content: doc.content, editLink: editLink,
                                    partials: {body: 'view.html'}} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var url = request.path;
  var content = getWikiContents(page, false, function(doc){
    response.render('layout.html', {title: page, content: doc.content, revision: doc._rev, url: url,
                                    partials: {body: 'edit.html'}} );
  });
});

app.post('/wiki/:page/edit', function(request, response){
  var page = request.params.page;

  var cancel = request.body.cancel;
  if("cancel" === cancel) return response.redirect('/wiki/'+page);

  var revision = request.body.revision;
  var content = request.body.content; //TODO: sanitize input
  var comment = request.body.comment;
  var preview = request.body.preview;

  if("preview" === preview){
    var error = request.body.error;
    if(error === 'Conflict Detected')
      return showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment,
                                                partials: {body: 'diff.html', editor: 'edit.html'}});
    else
      return response.render('layout.html', {title: page, content: content, revision: revision, comment: comment, html_content: processHtml(content), url: request.path,
                                    partials: {body: 'preview.html', editor: 'edit.html'}});
  }else{
    saveWikiContents(page, content, revision, comment, function(status){
      if(status === 'ok') return response.redirect('/wiki/'+page); //return to view
      else{ //display contents in editor
        if(status === "conflict"){
          return showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment,
                                                partials: {body: 'diff.html', editor: 'edit.html'}});
        }else{
          return response.render('layout.html', {title: page, content: content, revision: revision, url: request.path, error: 'Unable to save contents: ' + status, comment: comment,
                                          partials: {body: 'edit.html'}} );
        }
      }
    });
  }
});

app.post('/wiki/add', function(request, response){
  var page = request.body.title;
  response.redirect('/wiki/'+page+'/edit');
});

app.listen(8080, function(){
  console.log('listening...');
});
