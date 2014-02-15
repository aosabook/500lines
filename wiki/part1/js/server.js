var express = require('express'),
      fs = require('fs'),
      whiskers = require('whiskers'),
      marked = require('marked'),
      app = express();

app.use(express.bodyParser()); //for post parameter parsing
app.engine('.html', whiskers.__express);
app.set('views', __dirname+'/../views');

getSeparator = function(path){
  //if path already ends in /, return empty string, otherwise return /
  return /\/$/.test(path) ? '': '/';
}

processHtml = function(wikiMarkup){
  return marked(wikiMarkup);
}

getWikiContents = function(page, toHtml, callback){
  return fs.readFile("../files/"+page, "utf-8", function(err, data){
    if(!data) data = '';
    if(toHtml) data = processHtml(data);
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
  fs.readdir('../files/', function(err, files){
    if(err) return response.send(500, err);
    var addLink = request.path + getSeparator(request.path) + "add";
    response.render('list.html', {pages: files, url: addLink});
  });
});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  //read page if it exists
  var content = getWikiContents(page, true, function(content){
    var editLink = request.path + getSeparator(request.path) + "edit";
    //show contents of page
    response.render('view.html', {title: page, content: content, editLink: editLink} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  var page = request.params.page;
  var url = request.path;
  var content = getWikiContents(page, false, function(content){
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


app.post('/wiki/add', function(request, response){
  var page = request.body.title;
  response.redirect('/wiki/'+page+'/edit');
});

app.listen(8080, function(){
  console.log('listening...');
});
