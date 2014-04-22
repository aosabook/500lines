/**
	* 	A very basic model of HTTP
	*/
module http
-- for times etc
open dynamic
sig Host {}
sig Path {}

-- for now, omit port and protocol
sig Url {
	host : Host,
	path : lone Path
	}

sig Resource {}

abstract sig Endpoint {}

-- for now, express DNS as host field of server
sig Server extends Endpoint {
	host: Host,
	resource: Path -> lone Resource
	}
sig Client extends Endpoint {}

sig Time {}
sig Call {
	after, before: Time,
	arg, result: lone Resource
	}

sig HttpReq extends Call {
	from: Client,
	to: Server,
	url: Url
	} { to.host = url.host and result = to.resource[url.path] }

-- for now, same behavior
sig GetHttpReq, PostHttpReq extends HttpReq { }

run {some result}
