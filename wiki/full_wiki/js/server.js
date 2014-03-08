const config = require('./config.js'),
      util = require('./lib/util.js'),
      express = require('express'),
      whiskers = require('whiskers'),
      passport = require('passport'),
      LocalStrategy = require('passport-local').Strategy, //local authentication
      jsdiff = require('diff'),
      app = express();


var store;
if(config.useDBStore){
  console.log('Using DB store at ' + config.dbURL + '/' + config.dbName);
  store = require('./lib/db_store.js');
}else{
  console.log('Using file store at ' + config.fileStoreDir);
  store = require('./lib/file_store.js');
}

passport.use('local', new LocalStrategy(function(username, password,done){
  store.authenticate(username, password, function(err, user){
    if(err) return done(null, false, {message: err});
    if(user) return done(null, user);
    done(null, false, {message: 'Login failed'});
  });
}));


app.use(express.bodyParser()); //for post parameter parsing
app.use(express.cookieParser());
app.use(express.session({ secret: '500lineswiki' }));
app.use(passport.initialize());
app.use(passport.session());
app.engine('.html', whiskers.__express);
app.set('views', config.viewsdir);

passport.serializeUser(function(user, done) {
    done(null, user.name);
});

passport.deserializeUser(function(username, done) {
    store.getUser(username, done);
});

showCompareEditor = function(request, response, args){
  //get latest version of doc from couch
  store.getWikiContents(args.title, false, function(doc){
    //show contents of both pages
    args['comparecomment'] = doc.comment;
    args['username'] = doc.user;
    args['comparedate'] = util.formatDate(doc.updatedDate);
    args['revision'] = doc._rev;
    //fancy diff
    var usercontent = args.content;
    var savedcontent = doc.content;
    var diff = jsdiff.diffLines(usercontent, savedcontent);
    var diffContent = '';
    diff.forEach(function(part){
      var style = part.added ? 'added' : part.removed ? 'removed' : 'common';
      diffContent += '<span class="'+style+ '">' + part.value + '</span>';
    });
    args['comparecontent'] = diffContent;
    response.render('layout.html', args);
  });
}

app.get('/wiki', function(request, response){
  request.session.currentPage = request.path;
  store.listWikiPages(function(statusCode, error, content){
    if(statusCode != 200){
      response.send(statusCode, error);
    }else{
      var addLink = request.path + util.getSeparator(request.path) + "add";
      response.render('layout.html', {pages: JSON.parse(content).rows, url: addLink, user: request.user,
                                      partials: {body: 'list.html', login: 'login.html'}});
    }
  })
});

app.get('/wiki/:page', function(request, response){
  request.session.currentPage = request.path;
  var page = request.params.page;
  //read page if it exists
  store.getWikiContents(page, true, function(doc){
    //show contents of page
    doc.updatedDate = util.formatDate(doc.updatedDate);
    response.render('layout.html', {title: page, page: page, doc: doc, user: request.user,
                                    partials: {body: 'view.html', login: 'login.html'}} );
  });
});

app.get('/wiki/:page/edit', function(request, response){
  request.session.currentPage = request.path;
  if(!request.isAuthenticated()) response.redirect('/unauthorized/');
  var page = request.params.page;
  var url = request.path;
  store.getWikiContents(page, false, function(doc){
    response.render('layout.html', {title: page, content: doc.content, revision: doc._rev, url: url, user: request.user,
                                    partials: {body: 'edit.html', login: 'login.html'}} );
  });
});

app.post('/wiki/:page/edit', function(request, response){
  if(!request.isAuthenticated()) response.redirect('/unauthorized/');
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
      return showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment, user: request.user,
                                                partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
    else
      return response.render('layout.html', {title: page, content: content, revision: revision, comment: comment, html_content: util.processHtml(content), url: request.path, user: request.user,
                                    partials: {body: 'preview.html', editor: 'edit.html', login: 'login.html'}});
  }else{
    //save page
    store.saveWikiContents(page, content, revision, request.user, comment, function(status){
      if(status === 'ok') return response.redirect('/wiki/'+page); //return to view
      else{ //display contents in editor
        if(status === "conflict"){
          return showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment, user: request.user,
                                                partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
        }else{
          return response.render('layout.html', {title: page, content: content, revision: revision, url: request.path, error: 'Unable to save contents: ' + status, comment: comment, user: request.user,
                                          partials: {body: 'edit.html', login: 'login.html'}} );
        }
      }
    });
  }
});

app.post('/wiki/add', function(request, response){
  var page = request.body.title;
  response.redirect('/wiki/'+page+'/edit');
});

app.get('/loginfail', function(request, response){
  return response.render('layout.html', {title: 'Login failed', user: request.user,
                                         partials: {body: 'loginfail.html', login: 'login.html'}});
});

app.get('/unauthorized', function(request, response){
  return response.render('layout.html', {title: 'Unauthorized', user: request.user,
                                         partials: {body: 'unauthorized.html', login: 'login.html'}});
});

app.post('/login', function(request, response, next){
    var currentUrl = request.session.currentPage || '/wiki/';
    if(request.body.signup == "Sign Up"){ //sign up button clicked, insert new record in user store
      var username = request.body.username;
      var password = request.body.password;
      store.insertUser(username, password, function(status){
        if(status == 'ok') return passport.authenticate('local', { successRedirect: currentUrl, failureRedirect: '/loginfail' })(request, response, next);
        return response.render('layout.html', {title: 'Login failed', error: status, user: request.user,
                                         partials: {body: 'loginfail.html', login: 'login.html'}});
      });
    }else{
      passport.authenticate('local', { successRedirect: currentUrl, failureRedirect: '/loginfail' })(request, response, next);
    }
});

app.post('/logout', function(request, response){
  var currentUrl = request.session.currentPage || '/wiki/';
  request.logout();
  response.redirect(currentUrl);
});

app.listen(config.webserverport, function(){
  console.log("Server started. Visit http://localhost:" + config.webserverport + "/wiki/ to access the wiki.");
});
