var config = require('./config/config.js');
var express = require('express');
var whiskers = require('whiskers');
var passport = require('passport');

config.store.configureStore(function(err, store){
  if(err) throw err;
  require("./passport.js")(passport, store);

  var app = express();
  app.use(express.bodyParser());
  app.use(express.cookieParser());
  app.use(express.session({ secret: '500lineswiki' }));
  app.use(passport.initialize());
  app.use(passport.session());
  app.engine('.html', whiskers.__express);
  app.set('views', config.viewsdir);

  require("./wiki_routes.js")(app, store);
  require("./auth_routes.js")(app, store, passport);

  app.listen(config.webserverport, function(){
    console.log("Server started. Visit http://localhost:" + config.webserverport + "/wiki/ to access the wiki.");
  });
});

