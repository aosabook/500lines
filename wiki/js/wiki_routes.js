var jsdiff = require('diff');
var dateformat = require('dateformat');
var wiki_util = require('./wiki_util.js');

module.exports = function (app, store) {

  this.checkAuthenticated = function(request, response, next){
      if(!request.isAuthenticated()) response.redirect('/unauthorized/');
      next();
  }

  this.formatDate = function(date){
    return dateformat(date, "h:MMTT d-mmm-yyyy");
  }

  app.get('/wiki', function(request, response){
    request.session.currentPage = request.path;
    store.listWikiPages(function(error, content){
      if(error) response.send(500, error);
      response.render('layout.html', {pages: JSON.parse(content).rows, user: request.user, title: 'Wiki',
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
    //read page if it exists
    store.getWikiContents(page, true, function(doc){
      //show contents of page
      doc.updatedDate = this.formatDate(doc.updatedDate);
      response.render('layout.html', {title: page, page: page, doc: doc, user: request.user,
                                      partials: {body: 'view.html', login: 'login.html'}} );
    });
  });

  app.get('/wiki/:page/edit', this.checkAuthenticated, function(request, response){
    request.session.currentPage = request.path;
    var page = request.params.page;
    var url = request.path;
    store.getWikiContents(page, false, function(doc){
      response.render('layout.html', {title: page, content: doc.content, revision: doc._rev, url: url, user: request.user,
                                      partials: {body: 'edit.html', login: 'login.html'}} );
    });
  });

  app.post('/wiki/:page/edit', this.checkAuthenticated, function(request, response){
    var page = request.params.page;
    if("cancel" === request.body.cancel) return response.redirect('/wiki/'+page);

    var revision = request.body.revision;
    var content = request.body.content;
    var comment = request.body.comment;
    var preview = request.body.preview;

    if("preview" === preview){ //todo create new route for preview
      //show preview
      var error = request.body.error;
      if(error === 'Conflict Detected')
        return this.showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path, error: 'Conflict Detected', comment: comment, user: request.user,
                                                  partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
      else
        return response.render('layout.html', {title: page, content: content, revision: revision, comment: comment, html_content: wiki_util.processHtml(content), url: request.path, user: request.user,
                                      partials: {body: 'preview.html', editor: 'edit.html', login: 'login.html'}});
    }else{
      var args = {_id: page, content: content, comment: comment, user: request.user.name, _rev: revision};
      store.saveWikiContents(args, function(error, status){
        if(error){
          if(error.message === "conflict"){
            return this.showCompareEditor(request, response, {title: page, content: content, editrev: 'Your edits', url: request.path,
                                                              error: 'Conflict Detected', comment: comment, user: request.user,
                                                           partials: {body: 'diff.html', editor: 'edit.html', login: 'login.html'}});
          }else{
            return response.render('layout.html', {title: page, content: content, revision: revision, url: request.path,
                                                   error: 'Unable to save contents: ' + error.message, comment: comment, user: request.user,
                                                 partials: {body: 'edit.html', login: 'login.html'}} );
          }
        }else{
          return response.redirect('/wiki/'+page); //return to view
        }
      });
    }
  });

  this.showCompareEditor = function(request, response, args){
    //get latest version of doc from couch
    store.getWikiContents(args.title, false, function(doc){
      //show contents of both pages
      args.comparecomment = doc.comment;
      args.username = doc.user;
      args.comparedate = this.formatDate(doc.updatedDate);
      args.revision = doc._rev;
      //fancy diff
      var usercontent = args.content;
      var savedcontent = doc.content;
      var diff = jsdiff.diffLines(usercontent, savedcontent);
      var diffContent = '';
      diff.forEach(function(part){
        var style = part.added ? 'added' : part.removed ? 'removed' : 'common';
        diffContent += '<span class="'+style+ '">' + part.value + '</span>';
      });
      args.comparecontent = diffContent;
      response.render('layout.html', args);
    });
  };
};
