var jsdiff = require('diff');
var dateformat = require('dateformat');
var marked = require('marked');

module.exports = function (app, store) {

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
    response.render('layout.html', {title: 'Error', error: error, user: request.user, partials: {body: 'error.html', login: 'login.html'}});
  };

  app.get('/wiki', function(request, response){
    request.session.currentPage = request.path;
    store.listWikiPages(function(error, content){
      if(error) this.handleError(request, response, error);
      response.render('layout.html', {pages: content, user: request.user, title: 'Wiki',
                                      partials: {body: 'list.html', login: 'login.html'}});
    });
  });

  app.post('/wiki/add', function(request, response){
    var page = request.body.title;
    response.redirect('/wiki/'+page+'/edit');
  });

  app.get('/wiki/:page', function(request, response){
    request.session.currentPage = request.path;
    var page = request.params.page;
    store.getWikiContents(page, function(error, doc){
      if(error) return this.handleError(request, response, error);
      this.formatHtmlDoc(doc, function(err, formattedDoc){
        if(err) return this.handleError(request, response, err);
        response.render('layout.html', {title: page, page: page, doc: doc, user: request.user,
                                      partials: {body: 'view.html', login: 'login.html'}} );
      });
    });
  });

  app.get('/wiki/:page/edit', this.checkAuthenticated, function(request, response){
    request.session.currentPage = request.path;
    var page = request.params.page;
    var url = request.path;
    store.getWikiContents(page, function(error, doc){
      if(error) return this.handleError(request, response, error);
      response.render('layout.html', {title: page, content: doc.content, revision: doc._rev, url: url, user: request.user,
                                      partials: {body: 'edit.html', login: 'login.html'}} );
    });
  });

  app.post('/wiki/:page/edit', this.checkAuthenticated, function(request, response){
    var page = request.params.page;
    if(request.body.cancel === "cancel") return response.redirect('/wiki/'+page);

    var revision = request.body.revision;
    var content = request.body.content;
    var comment = request.body.comment;

    if(request.body.preview === "preview"){ //todo create new route for preview
      var error = request.body.error;
      if(error === 'Conflict Detected')
        return this.showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment, user: request.user,
                                                  partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
      else
        this.formatHtmlString(content, function(error, htmlContent){
          if(error) return this.handleError(request, response, error);
          return response.render('layout.html', {title: page, content: content, revision: revision, comment: comment, html_content: htmlContent, url: request.path, user: request.user,
                                      partials: {body: 'preview.html', editor: 'edit.html', login: 'login.html'}});
        });
    }else{
      var args = {_id: page, content: content, comment: comment, user: request.user.name};
      if(revision) args._rev = revision;
      store.saveWikiContents(args, function(error, status){
        if(error && error.message === "conflict")
            return this.showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path,
                                                              error: 'Conflict Detected', comment: comment, user: request.user,
                                                           partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
        else if(error)
            return response.render('layout.html', {title: page, content: content, revision: revision, url: request.path,
                                                   error: 'Unable to save contents: ' + error.message, comment: comment, user: request.user,
                                                 partials: {body: 'edit.html', login: 'login.html'}} );
        return response.redirect('/wiki/'+page); //return to view
      });
    }
  });

  this.showCompareEditor = function(request, response, args){
    store.getWikiContents(args.title, function(error, doc){
      if(error){
        args.error = args.error + error;
        doc = {};
        doc.content = 'Error';
      }
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
      response.render('layout.html', args);
    });
  };
};
