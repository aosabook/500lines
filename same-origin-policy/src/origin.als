/**
  *  origin.als
  *    A model of the notion of origin per the SOP
  */
module origin

open http
open browser

// An origin is defined as a triple (protocol, host, port) where port is
// optional
sig Origin {
  protocol: Protocol,
  host: Domain,
  port: lone Port
}

fun origin[u: Url] : Origin {
  {o: Origin | o.protocol = u.protocol and o.host = u.host and o.port = u.port }
}

fun origin[u: Url, h: Domain] : Origin {
  {o: Origin | o.protocol = u.protocol and o.host = h and o.port = u.port }
}

run {}
