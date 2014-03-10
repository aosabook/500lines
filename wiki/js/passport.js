module.exports = function passportSetup(passport, store) {

  var LocalStrategy = require('passport-local').Strategy; //local authentication

  passport.use('local', new LocalStrategy(function(username, password, done){
    store.authenticate(username, password, function(err, user){
      if(err) return done(null, false, {message: err});
      if(!user) return done(null, false, {message: 'Login failed'});
      return done(null, user);
    });
  }));

  passport.serializeUser(function(user, done) {
    done(null, user.name);
  });

  passport.deserializeUser(function(username, done) {
    store.getUser(username, done);
  });
};
