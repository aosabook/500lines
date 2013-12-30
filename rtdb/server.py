import urllib, os, wsgiref.simple_server
import webob, webob.dec, webob.static, webob.exc, webob.byterange

cors_headers = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, PUT, OPTIONS",
    "Access-Control-Allow-Headers": "Range, Content-Range, If-None-Match, If-Matches, Content-Type",
    }


class Server(object):

    def __init__(self, dir):
        self.dir = dir
        if not os.path.exists(self.dir):
            os.makedirs(self.dir)
        self.static = webob.static.DirectoryApp(os.path.dirname(__file__))

    @webob.dec.wsgify
    def __call__(self, req):
        if req.method == "OPTIONS":
            return webob.Response(headers=cors_headers, body="")
        resp = req.send(self.app)
        resp.headers.update(cors_headers)
        return resp

    @webob.dec.wsgify
    def app(self, req):
        if req.path_info_peek() == "static":
            req.path_info_pop()
            return self.static
        filename = os.path.join(self.dir, urllib.quote(req.path_info.strip("/"), ""))
        if not os.path.exists(filename):
            etag = None
        else:
            etag = str(os.path.getmtime(filename))
        if req.method == "GET" or req.method == "HEAD":
            return webob.static.FileApp(filename, etag=etag, content_type="application/octet-stream")
        elif req.method == "PUT":
            if etag not in req.if_match:
                return webob.exc.HTTPPreconditionFailed()
            content_range = webob.byterange.ContentRange.parse(req.headers.get('content-range') or '')
            if content_range:
                if not os.path.exists(filename):
                    return webob.exc.HTTPNotFound()
                fp = open(filename, "r+b")
                fp.seek(content_range.start)
                # FIXME: doesn't handle odd ranges
                fp.write(req.body)
            elif req.headers.get("content-range"):
                return webob.exc.HTTPBadRequest()
            else:
                fp = open(filename, "wb")
                fp.write(req.body)
            fp.close()
            etag = str(os.path.getmtime(filename))
            return webob.exc.HTTPNoContent(etag=etag)
        else:
            return webob.exc.HTTPMethodNotAllowed(allow="GET,HEAD,PUT")

if __name__ == "__main__":
    host = os.environ.get("HOST", "127.0.0.1")
    port = int(os.environ.get("PORT", 8080))
    print("Serving on http://%s:%s" % (host, port))
    server = wsgiref.simple_server.make_server(host, port, Server(os.environ.get("DIR", "./data")))
    server.serve_forever()
