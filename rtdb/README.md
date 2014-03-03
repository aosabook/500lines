## Realtime Database (or sync?)

This is a demonstration of doing real-time sync, which is (or can be) the basis for a real-time database, similar to something like [Firebase](https://www.firebase.com/) or (one aspect of) [Meteor](https://www.meteor.com/).  In this case though we only support complete synchronization of datasets between clients based on an agreed-upon resource, no partial results or querying is allowed for.

The system includes client and server.

### Server

The server attempts to be minimal, supporting normal (including ranged) GET requests (using [WebOb's](http://webob.org/) static resource handling), and then a special case of PUT with Content-Range to allow writing byte ranges (and appending).

Though theoretically reasonable to allow Content-Range with PUT, it's a somewhat obscure combination, and maybe one that doesn't make sense. [PATCH](http://tools.ietf.org/html/rfc5789) might be more sensible.

The server as at least initially implemented is only safe for single-process access, as it does not do any file locking.  File locking seems somewhat beside the point, but wouldn't be terribly hard (or long) to implement.

The server doesn't include any functionality besides the special PUT support that is specific to the protocol.

I opted not to use WebSockets, though they would of course seem like a reasonable choice, because the logic of XMLHttpRequest polling is necessary (since connections are intermittent) and adding streaming updates to that seemed like too much.

The server is entirely in `server.py`, with WebOb as the only dependency.

### Client

This is the actually interesting part.

The basic model is simply to append updates to a single resource, and other clients look for those updates.  You cannot append an update unless you are fully up-to-date.

Resources have two sorts of IDs.  The ETag is used as conventionally with HTTP, and is used when appending updates to ensure that the client is up-to-date.  This ETag changes everytime there is an addition to the resource.

Resources also have a collection ID, which is used to identify the sequence.  Anything you know about the sequence of updates is invalid if someone resets the sequence, or you are pointed at a different sequence.  So this collection ID is always kept at the end of the resource, as the last 8 bytes.  This detects conflicts even as the resource is updated.

The stream itself is a list of JSON objects.  Each object is preceded by a 6-digit hex length of the JSON object.  All JSON is strictly ASCII, so that we don't have to deal with a mismatch between bytes (which are used for the HTTP range requests and PUTs) and Javascript strings, which could include Unicode characters.

The client interacts with the server with just four methods: one for incoming updates, one to get pending updates, one to mark pending updates as having been uploaded, and one when all information about what is saved should be reset.

Conflicts are handled by the first one to the server "wins".  But because we just stream updates, it's up to the application to actually handle any conflicts.  You can overwrite or ignore incoming updates if you want.  You could also try to incorporate the conflict into the object and subsequent update, and then allow the user to resolve the conflict later.

You might notice the resource will increase in size indefinitely.  A "garbage collection" step would be reasonable, which is where the collection ID also becomes handy.  A client would at some point decide a collection was necessary, and reconstruct the resource in its entirety, uploading the new resource with a new collection ID.
