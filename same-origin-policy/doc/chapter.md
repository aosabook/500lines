#Same-Origin Policy 

The same-origin policy (SOP) is a collection of mechanisms designed to restrict interactions between resources from two different origins. Originally introduced into Netscape Navigator, the SOP has played a critical role in the security of web applications; without it, it would be far easier for a malicious hacker to browse your private photos on Facebook, or empty the balance on your bank account.

However, the SOP is far from perfect. At times, it is too restrictive; there are legitimate cases in which two documents from different origins should be able to share a resource (e.g., mashups). At other times, it is not restrictive enough, with corner cases that can be exploited for common attacks such as cross-site request forgery (CSRF). Furthermore, the design of the SOP has evolved organically over the years and still puzzles many developers to this date.

This chapter is somewhat different from others in this book. Instead of building a working implementation of a browser with the same origin policy, our goal is to construct a concrete, executable _model_ that serves as a simple yet precise documentation of the SOP. Like an implementation, the model can be executed to explore dynamic behaviors of the system; but unlike an implementation, the model omits low-level details that often get in the way of understanding the essential core of the SOP. 

To construct this model, we use _Alloy_, a language for modeling and analyzing software design. An Alloy model cannot be executed in the traditional sense of program execution. Instead, a model can be (1) _simulated_ to produce an _instance_, which represents a valid configuration or a trace, and (2) _checked_ to see whether the model satisfies a desired _property_.

[TODO: Explain: What's the point of building a model when you can just build implementation?]

## Model

We will start by building a small model of the web, along the way introducing features of Alloy and exploring different types of analyses that can be performed with it. We will then extend the model by adding the SOP and other mechanisms that have been invented to cope with the limtations of the SOP.

### Modeling Servers and Clients
In essence, an Alloy model contains a universe of _objects_ and _relations_ among them. An Alloy _signature_ defines a set of objects, and a _field_ introduces a relation between two or more types of objects. Here is a simple model describing a web server that maps URLs to resources:
```
sig URL {}                  // signature that introduces a set of URL objects
sig Resource {}
sig Server {
   resMap : URL -> Resource    // field that maps URLs to resources 
}
```
The snippet above constitutes a working Alloy model, and can be immediately executed to produce an instance using the _run_ command:
```
run {} for 3 
```
Executing this command instructs the Alloy Analyzer to generate an instance with up to three servers, URLs, and resources.

[TODO: Include a figure that shows an instance with two servers that map a common URL to some resource]

A closer look at the instance shows that the two servers share a common URL in their resource map. This is strange, since one would expect each URL points to a unique location on the web. To fix this, we can add a _constraint_ that says "no two distinct servers (**s1** and **s2**) can map a common URL (**u**) to one or more resources", and put this constraint inside a _predicate_:
```
pred invariant {           // predicate named "invariant"
    no disj s1, s2 : Server | some u : URL | some s1.resMap[u] and some s2.resMap[u] 
}
run { invariant } for 3
```
When one or more predicates are provided as part of a _run_ command, the analyzer will only generate instances that satisfy all of the constraints in the predicates.

[TOOD: Need more text here.]
```
sig Browser {
    frames : set Frame      // each browser has a set of frames
}
sig Frame {
    script : lone Script    // each frame may have a script running inside it
}
sig Script {}
```
### Adding Messages

So far, we have focused on modeling the static structure of the web -- servers, clients, resources, etc. Now we will make the model a little more dynamic, by describing how different parts of the web communicate to each other through messages.

To start, we introduce a set of messages into the model. Each message is associated with a sender and a receiver module: 
```
abstract sig Msg {
    sender, receiver : Module
}
abstract sig Module {}
```
An _abstract_ signature introduces a subtyping relationship with another signature that extends it. For example, we may say that there are two specific types of messages between a server and a client: HTTP requests and responses. Each request is associated with an URL, and a response contains the resource to which a requested URL points. Furthermore, each subtype inherits all of the fields from the supertype, and so each HTTP message is assigned a sender and a receiver:
```
sig HTTPReq extends Msg {       // HTTP requests
    url : URL
}
sig HTTPResp extends Msg {      // HTTP responses
    res : Resource
}
```
However, this isn't quite enough; there is nothing in this model that says a HTTP response cannot be sent by a browser to a server, or to itself! A _signature fact_, placed immediately following field declarations, is a constraint that applies to every object in that signature. We can use a signature fact to say that every HTTP response is sent by a server to a browser (and similarly for requests, too):
```
sig Server extends Module { ... }
sig Browser extends Module { ... }
sig HTTPReq extends Msg { ... } {
    sender in Browser and receiver in Server
}
sig HTTPReq extends Msg { ... } {
    sender in Receiver and receiver in Browser
}
```
Actual communication between a client and a server, of course, is more complicated in reality than what is depicted here. Our model omits details such as underlying TCP/IP connections, message headers, intermediate network nodes, and many others, since these details are not important to our understanding of the SOP at the application level (however, if you are concerned about low-level network attacks, then you may eventually want to add those details). 
```
abstract sig APICall extends Msg {}{
    sender in Script
    receiver in Browser
}
```

### Analyzing the Model

We have already discussed how the Alloy Analyzer can be used to generate valid instances of a system. Another type of analysis is _checking_ whether a model satisfies a property. 

Since our primary concern in this chpater is security, let us consider one property that is desirable in many web applications: _critical resources should never be accessible to malicious modules_. Before stating this property precisely in Alloy, we first designate some subsets of resources and modules to be _critical_ and _malicious_, respectively. Furthermore, we extend the  **Module** signature with a set of resources that a module accesses: 
```
sig CriticalResource in Resource {}     // some subset of resources are critical
abstract sig Module {
    accesses : set Resource
}
sig MaliciousModule in Module {}
```
Then, we state our security property as an _assertion_ and _check_ whether it holds true over every instance of the model:
```
assert noResourceLeak {
    no r : CriticalResource, b : MaliciousModule | r in b.accesses 
}
check noResourceLeak for 5
```
When executed, _check_ command instructs the analyzer to look for a _counterexample_ -- a system trace that represents a violation of the property. In our case, a counterexample, if it exists would consist of a scenarion in which a malicious script or server accesses a piece of critical resource.

The analysis is _exhaustive_, in that it will consider every possible configuration and behavior of the system (up to the specified bound), and so if there is a counterexample to the property, then the analyzer is guaranteed to find it.

### Modeling the SOP

We can now build on top of the basic web model to describe the same-origin policy.

### Bypassing the SOP

[TODO]

### Cross-Origin Resource Sharing (CORS)

[TODO]

## Conclusion

[TODO]

To fully implement the SOP would have surely required more than 500 lines of code, and probably involve not only a browser but the server as well as a communication network.
