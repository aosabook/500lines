var config = require('./config.js');
var express = require('express');
var whiskers = require('whiskers');
var passport = require('passport');

var dbStore = require('./db_store.js')(config.dbURL, config.dbName);
require("./passport.js")(passport, dbStore);

var app = express();
app.use('/public', express.static(__dirname + '/public'));
app.set('views', __dirname + '/views');
app.engine('.html', whiskers.__express);
app.use(express.bodyParser());
app.use(express.cookieParser());
app.use(express.session({ secret: '500lineswiki' }));
app.use(passport.initialize());
app.use(passport.session());

require("./wiki_routes.js")(app, dbStore);
require("./auth_routes.js")(app, dbStore, passport);

exports.server = app.listen(config.webserverport, function(){
    console.log("Server started. Visit http://localhost:" + config.webserverport + "/wiki/ to access the wiki.");
});

exports.shutDown = function serverShutdown(){
  exports.server.close();
};
