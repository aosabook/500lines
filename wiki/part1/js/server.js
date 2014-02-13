var express = require('express'),
      fs = require('fs'),
      whiskers = require('whiskers'),
      app = express();

app.use(express.bodyParser()); //for post parameter parsing
app.engine('.html', whiskers.__express);
app.set('views', __dirname+'/../views');

getWikiContents = function(page, callback){
  return fs.readFile("../files/"+page, function(err, data){
    callback(data);
  });
}

saveWikiContents = function(page, contents, callback){
  fs.writeFile('../files/'+page, contents, function(err){
    if(err) return callback(false);
    callback(true);
  });
}

app.get('/wiki/', function(request, response){
  console.log('got view request for wiki index');
  //get listing of available pages from disk
  //for each page, render a link
  fs.readdir('../files/', function(err, files){
    if(err) return response.send(500, err);
    response.render('list.html', {pages: files});
  });
});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  //read page if it exists
  var content = getWikiContents(page, function(content){
    if(!content) content = '';
    var sep = /\/$/.test(request.path) ? '': '/';
    var editLink = request.path + sep + "edit";
    //show contents of page
    response.render('view.html', {title: page, content: content, editLink: editLink} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var url = request.path;
  var content = getWikiContents(page, function(content){
    if(!content) content = ''; //no contents yet
    response.render('edit.html', {title: page, content: content, url: url} );
  });
});

app.post('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var content = request.body.content; //TODO: sanitize input
  saveWikiContents(page, content, function(success){
    if(success) response.redirect('/wiki/'+page); //return to view
    else{ //display contents in editor
      response.render('edit.html', {title: page, content: content, url: request.path, error:'Unable to save contents'} );
    }
  });
});

app.listen(8080, function(){
  console.log('listening...');
});
