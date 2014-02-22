const config = require('./config.js'),
      store = require('./store.js'),
      util = require('./util.js'),
      express = require('express'),
      whiskers = require('whiskers'),
      dateformat = require('dateformat'),
      app = express();

app.use(express.bodyParser()); //for post parameter parsing
app.engine('.html', whiskers.__express);
app.set('views', __dirname+'/../views');

showCompareEditor = function(request, response, args){
  //get latest version of doc from couch
  store.getWikiContents(args.title, false, function(doc){
    //show contents of both pages
    args['comparecontent'] = doc.content;
    args['comparecomment'] = doc.comment;
    args['comparedate'] = dateformat(doc.updatedDate, "h:MMTT d-mmm-yyyy");
    args['revision'] = doc._rev;
    response.render('layout.html', args);
  });
}

app.get('/wiki', function(request, response){
  store.listWikiPages(function(statusCode, error, content){
    if(statusCode != 200){
      response.send(statusCode, error);
    }else{
      var addLink = request.path + util.getSeparator(request.path) + "add";
      response.render('layout.html', {pages: JSON.parse(content).rows, url: addLink,
                                   partials: {body: 'list.html'}});
    }
  })
});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  //read page if it exists
  store.getWikiContents(page, true, function(doc){
    var editLink = request.path + util.getSeparator(request.path) + "edit";
    //show contents of page
    response.render('layout.html', {title: page, content: doc.content, editLink: editLink,
                                    partials: {body: 'view.html'}} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var url = request.path;
  var content = store.getWikiContents(page, false, function(doc){
    response.render('layout.html', {title: page, content: doc.content, revision: doc._rev, url: url,
                                    partials: {body: 'edit.html'}} );
  });
});

app.post('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  if("cancel" === request.body.cancel) return response.redirect('/wiki/'+page);

  var revision = request.body.revision;
  var content = request.body.content;
  var comment = request.body.comment;
  var preview = request.body.preview;

  if("preview" === preview){
    //show preview
    var error = request.body.error;
    if(error === 'Conflict Detected')
      return showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment,
                                                partials: {body: 'diff.html', editor: 'edit.html'}});
    else
      return response.render('layout.html', {title: page, content: content, revision: revision, comment: comment, html_content: util.processHtml(content), url: request.path,
                                    partials: {body: 'preview.html', editor: 'edit.html'}});
  }else{
    //save page
    store.saveWikiContents(page, content, revision, comment, function(status){
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

app.listen(config.webserverport, function(){
  console.log("Server started. Visit http://localhost:" + config.webserverport + "/wiki/ to access the wiki.");
});
