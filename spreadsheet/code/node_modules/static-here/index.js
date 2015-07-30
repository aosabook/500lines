var fs = require('fs'),
    http = require('http'),
    path = require('path'),
    url = require('url'),
    exec = require('child_process').exec,
    mime = require('mime');

var args = {},
    argv = process.argv.slice(2);

// Guess arguments.
for (var i = 0; i < argv.length; i++){
  arg = argv[i];
  if (arg.match(/^\d+$/)){
    args.port = arg;
  } else {
    args.host = arg;
  }
}

// Simple http response.
function httpRespond(res, code, txt, headers) {
  headers = headers || {};
  txt = txt || '';
  headers['Content-Type'] = "text/plain";
  res.writeHead(code, headers);
  res.end(txt);
}

var httpCb = function (req, res) {
  var uri = url.parse(req.url).pathname,
      filename = path.join(process.cwd(), uri);

  fs.exists(filename, function (exists) {
    if (!exists) {
      httpRespond(res, 404, "Page Not Found!\n");
      return;
    }
    
    if (fs.statSync(filename).isDirectory()) {
      if (filename.slice(-1) !== '/') {
        // Directory with out a trailing slash.
        // redirect http://host/directory to http://host/directory/
        httpRespond(res, 302, 'Location is a folder, redirecting..', {
          'Location': uri + '/'
        });
        return;
      } else {
        filename = path.join(filename, 'index.html');
      }
    }
    
    fs.readFile(filename, 'binary', function (err, file) {
      if (err) {
        httpRespond(res, 500, err + '\n');
        return;
      }
      var ext = path.extname(filename).slice(1);
      res.writeHead(200, {'Content-Type': mime.lookup(ext)});
      res.write(file, 'binary');
      res.end();
    });
  });
};

// Assign defaults and define the start server action.
args.port = args.port || 8888;
args.host = args.host || '0.0.0.0';
http.createServer(httpCb).listen(args.port, args.host);
console.log(
  'Serving files from %s at http://%s:%s/',
  process.cwd(),
  args.host,
  args.port
);
