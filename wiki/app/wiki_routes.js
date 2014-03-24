var jsdiff = require('diff');
var dateformat = require('dateformat');
var marked = require('marked');

module.exports = function (app, store) {

  //helper methods
  this.checkAuthenticated = function(request, response, next){
      if(!request.isAuthenticated()) response.redirect('/unauthorized/');
      next();
  };

  this.formatDate = function(date){
    if(!date) return '';
    return dateformat(date, "h:MMTT d-mmm-yyyy");
  };

  this.formatHtmlString = function(content, callback){
    try{
      content = marked(content, {sanitize: true});
      return callback(null, content);
    }catch(e){
      return callback(e, null);
    }
  };

  this.formatHtmlDoc = function(doc, callback){
    if(!doc) doc = {};
    if(!doc.content) doc.content = '';
    this.formatHtmlString(doc.content, function(error, formattedContent){
      if(error) return callback(error);
      doc.content = formattedContent;
      if(doc.updatedDate) doc.updatedDate = this.formatDate(doc.updatedDate);
      return callback(null, doc);
    });
  };

  this.handleError = function(request, response, error){
    console.error(error);
    response.send(500, error);
  };

  //main page routes
  app.get('/wiki', function(request, response){
    request.session.currentPage = request.path;
    request.session.currentQuery = null;
    response.render('layout.html', {handler: 'listWikiPages', user: request.user, title: 'Wiki', partials: {login: 'login.html', content: 'list.html'}});
  });

  app.get('/wiki/view', function(request, response){
    var page = request.query.page;
    request.session.currentPage = request.path;
    request.session.currentQuery = '?page='+page;
    response.render('layout.html', {handler: 'viewPage', page: page, user: request.user, title: 'Wiki', partials: {login: 'login.html', content: 'view.html'}});
  });

  app.get('/wiki/edit', this.checkAuthenticated, function(request, response){
    var page = request.query.page;
    request.session.currentPage = request.path;
    request.session.currentQuery = '?page='+page;
    response.render('layout.html', {handler: 'showEditPage', page: page, user: request.user, title: 'Wiki', partials: {login: 'login.html', content: 'edit.html'}});
  });

  app.post('/wiki/add', function(request, response){
    var page = request.body.title;
    response.redirect('/wiki/edit?page='+page);
  });

  //AJAX routes
  app.get('/listWikiPages', function(request, response){
    store.listWikiPages(function(error, content){
      if(error) this.handleError(request, response, error);
      //write back AJAX response to page
      response.contentType('json');
      response.send({ pages: content });
    });
  });

  app.get('/getWikiContent', function(request, response){
    var page = request.query.page;
    var noHtml = request.query.noHtml;
    store.getWikiContents(page, function(error, doc){
      if(error) return this.handleError(request, response, error);
      response.contentType('json');
      if(noHtml) return response.send({doc: doc});
      else this.formatHtmlDoc(doc, function(err, formattedDoc){
        if(err) return this.handleError(request, response, err);
        response.send({doc: formattedDoc});
      });
    });
  });

  app.post('/preview', function(request, response){
    var content = request.body.content;
    this.formatHtmlString(content, function(error, htmlContent){
      if(error) return this.handleError(request, response, error);
      response.contentType('json');
      response.send({preview: htmlContent});
    });
  });

  app.post('/save', this.checkAuthenticated, function(request, response){
    var page = request.body.page;
    var content = request.body.content;

    var args = {_id: page, content: content, comment: request.body.comment, user: request.user.name};
    if(request.body.revision) args._rev = request.body.revision;
    store.saveWikiContents(args, function(error, status){
      if(error && error.message === "conflict")
        return this.showCompareEditor(request, response, {page: page, content: content, error: 'Conflict Detected'});
      else if(error) return this.handleError(request, response, error);
      response.send({status: 'saved'});
    });
  });

  this.showCompareEditor = function(request, response, args){
    store.getWikiContents(args.page, function(error, doc){
      if(error) this.handleError(request, response, error);
      args.comparecomment = doc.comment;
      args.username = doc.user;
      args.comparedate = this.formatDate(doc.updatedDate);
      args.revision = doc._rev;
      var diff = jsdiff.diffLines(args.content, doc.content);
      args.comparecontent = '';
      diff.forEach(function(part){
        var style = part.added ? 'added' : part.removed ? 'removed' : 'common';
        args.comparecontent += '<span class="'+style+ '">' + part.value + '</span>';
      });
      response.contentType('json');
      response.send(409, args);
    });
  };

  app.get('/getUser', function(request, response){
    response.contentType('json');
    if(request.user)
      response.send({user: request.user.name});
    else
      response.send({user: null});
  });
};
