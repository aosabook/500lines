module origin

open http
open browser

// An origin is defined as a triple (protocol, host, port) where port is optional
sig Origin {
	protocol : Protocol,
	host : Domain,
	port : lone Port
}

fun url2origin[u : URL] : Origin {
	{o : Origin | o.host = u.host and o.protocol = u.protocol and o.port = u.port }
}

run {}
