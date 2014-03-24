import requests
response = requests.get("http://aosabook.org/en/posa/introduction.html")
print 'status code:', response.status_code
print 'content length:', response.headers['content-length']
print response.text
