module.exports = function (app, store, passport) {

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
      passport.authenticate('local', { successRedirect: currentUrl, failureRedirect: '/loginfail' })(request, response, next);
  });

  app.post('/launchsignup', function(request, response, next){
    var currentUrl = request.session.currentPage || '/wiki/';
    return response.render('layout.html', {title: 'Sign up', partials: {body: 'signup.html', login: 'login.html'}});
  });

  app.post('/signup', function(request, response, next){
    var currentUrl = request.session.currentPage || '/wiki/';
    store.insertUser(request.body.username, request.body.password, request.body.email, function(error){
      if(error){
        if(error.message == 'conflict') error.message = "Username already exists";
        return response.render('layout.html', {title: 'Login failed', error: error.message, user: request.user,
                                             partials: {body: 'loginfail.html', login: 'login.html'}});
      }
      return passport.authenticate('local', { successRedirect: currentUrl, failureRedirect: '/loginfail' })(request, response, next);
    });
  });

  app.post('/logout', function(request, response){
    var currentUrl = request.session.currentPage || '/wiki/';
    request.logout();
    response.redirect(currentUrl);
  });
};
