const express = require('express'),
      fs = require('fs'),
      app = express();

app.use(express.bodyParser());

getWikiContents = function(page, callback){
  return fs.readFile("../files/"+page, function(err, data){
    callback(data);
  });
}

saveWikiContents = function(page, contents, callback){
  fs.writeFile('../files/'+page, contents, function(err){
    if(err) throw err;
    callback(true);
  });
}

app.get('/wiki/', function(request, response){
  console.log('got view request for wiki index');
  //get listing of available pages from disk
  //for each page, render a link
  fs.readdir('../files/', function(err, files){
    if(err) response.send(500, err);
    var html = "<html><head><title>wiki</title></head><body><h1>Existing pages</h1>";
    for(var i=0; i<files.length; i++){
      html += '<a href="/wiki/'+files[i]+'" >' + files[i] + "</a><br/>";
    }
    html += "</body></html>";
    response.send(200, html);
  });

});

app.get('/wiki/:page', function(request, response){
  var page = request.params.page;
  console.log('got view request for page ' + page);

  //read page from disk if it exists
  var content = getWikiContents(page, function(content){
    if(!content) content = '';
    //add edit link
    var sep = /\/$/.test(request.path) ? '': '/';
    var editLink = request.path + sep + "edit";
    content += '<br/><a href="'+editLink+'">Edit</a>';
    //show contents of page
    response.send(200, '<html><head><title>'+page+'</title></head>'
                  +'<body><h1>'+page+'</h1>'+content+'</body></html>');
  });

});

app.get('/wiki/:page/edit', function(request, response){
  console.log('got edit request');
  //show editor
  var page = request.params.page;
  var url = request.path;
  var content = getWikiContents(page, function(content){
    if(!content) content = ''; //no contents yet
    var editor = '<form action="'+url+'" method="post">'
    +'<textarea name="content" style="width:80%;height:80%">'+content+'</textarea>'
    +'<button type="submit">Save</button></form>';
    response.send(200, '<html><head><title>'+page+'</title></head>'
                +'<body><h1>'+page+'</h1>'+editor+'</body></html>');
  });


});

app.post('/wiki/:page/edit', function(request, response){
  console.log('got new edit');
  //save edits
  var page = request.params.page;
  var content = request.body.content;
  saveWikiContents(page, content, function(success){
    //return to view
    if(success) response.redirect('/wiki/'+page);
    else{ //display contents in editor
      var editor = '<form action="'+request.path+'" method="post">'
      +'<textarea name="content" style="width:80%;height:80%">'+content+'</textarea>'
      +'<button type="submit">Save</button></form>';
      editor += "<p>Unable to save contents</p>";
      response.send(200, '<html><head><title>'+page+'</title></head>'
                  +'<body><h1>'+page+'</h1>'+editor+'</body></html>');
    }
  });


});

app.listen(8080, function(){
  console.log('listening...');
});
