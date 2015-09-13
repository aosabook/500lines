# testquery.py : CGI script that echoes query parameters.

import time, cgi, os

rawData = os.environ.get("QUERY_STRING", "")
params = dict(cgi.parse_qs(rawData))

print "Content-type: text/html"
print
print "<html>"
print "<body>"
print "<h1>Query parameters as of %s</h1>" % time.asctime()
if not params:
    print "<p>No parameters</p>"
else:
    print "<p>%d parameter(s)</p>" % len(params)
    print '<table cellpadding="3" border="1">'
    keys = params.keys()
    keys.sort()
    for k in keys:
        v = cgi.escape('|'.join(params[k]))
        k = cgi.escape(k)
        print "<tr><td>%s</td><td>%s</td></tr>" % (k, v)
    print "</table>"
print "</body>"
print "</html>"
