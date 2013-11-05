# testquery.py : CGI script that echoes query parameters.

import time, cgiutil

rawData = cgiutil.getRawCgiData()
params = cgiutil.parseCgiData(rawData)

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
        v = cgiutil.htmlEncode('|'.join(params[k]))
        k = cgiutil.htmlEncode(k)
        print "<tr><td>%s</td><td>%s</td></tr>" % (k, v)
    print "</table>"
print "</body>"
print "</html>"
