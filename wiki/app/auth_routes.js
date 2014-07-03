module.exports = function (app, store, passport) {

  this.doLogin = function(request, response, next){
    passport.authenticate('local', function(err, user, info){
        if(err) return next(err);
        if(!user) return response.send(401, 'User not found');
        request.logIn(user, function(err){
          if(err) return next(err);
          return response.send({user: user, path: (request.session.currentPage || '/wiki/')});
        });
      })(request, response, next);
  };

  app.get('/unauthorized', function(request, response){
    var error = 'You must log in to edit the wiki.  Please log in or create a new account using the login form above.';
    return response.render('layout.html', {title: 'Unauthorized', error: error, partials: {content: 'error.html'}});
  });

  app.post('/login', function(request, response, next){
    this.doLogin(request, response, next);
  });

  app.post('/signup', function(request, response, next){
    if(!request.body.username || !request.body.password) response.send(400, "Please enter a username and password.");
    store.insertUser(request.body.username, request.body.password, request.body.email, function(error){
      if(error && error.message == 'conflict') return response.send(400, "Username already exists");
      if(error) return response.send(400, error.message);
      this.doLogin(request, response, next);
    }.bind(this));
  });

  app.post('/logout', function(request, response){
    request.logout();
    response.send({status: 'ok'});
  });

  app.get('/getUser', function(request, response){
    if(request.isAuthenticated() && request.user) response.send({user: request.user.name});
    else response.send({user: null});
  });
};
