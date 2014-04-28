module origin

open http
open browser

// An origin is defined as a triple (protocol, host, port) where port is optional
sig Origin {
	protocol : browser/Protocol,
	host : http/Host,
	port : lone browser/Port
}

fun url2origin[u : browser/URL] : Origin {
	{o : Origin | o.host = u.host and o.protocol = u.protocol and o.port = u.port }
}

run {}
