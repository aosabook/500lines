module.exports = function (app, store, passport) {

  var doLogin = function(request, response, next){
    passport.authenticate('local', function(err, user, info){
        if(err) return next(err);
        if(!user) return response.send(401, 'User not found');
        request.logIn(user, function(err){
          if(err) return next(err);
          return response.send({user: user});
        });
      })(request, response, next);
  };

  app.get('/unauthorized', function(request, response){
    return response.render('layout.html', {handler: 'unauthorized', user: request.user, partials: {login: 'login.html'}});
  });

  app.post('/login', function(request, response, next){
      doLogin(request, response, next);
  });

  app.post('/signup', function(request, response, next){
    store.insertUser(request.body.username, request.body.password, request.body.email, function(error){
      if(error){
        if(error.message == 'conflict') error.message = "Username already exists";
        return response.send(400, error.message);
      }
      doLogin(request, response, next);
    });
  });

  app.post('/logout', function(request, response){
    request.logout();
    response.send({status: 'ok'});
  });

};
