# Exploring the Same Origin Policy with Alloy

## Introduction

The same-origin policy (SOP) is part of the security mechanism of every modern browser. It controls when scripts running in a browser can communicate with one another: roughly, only when they originate from the same website. First introduced in Netscape Navigator, the SOP now plays a critical role in the security of web applications; without it, it would be far easier for a malicious hacker to peruse your private photos on Facebook, or empty the balance on your bank account.

But the SOP is far from perfect. At times, it is too restrictive; there are cases (such as mashups) in which scripts from different origins should be able to share a resource but cannot. At other times, it is not restrictive enough, leaving corner cases that can be exploited using common attacks such as cross-site request forgery (CSRF). Furthermore, the design of the SOP has evolved organically over the years and puzzles many developers.

This chapter is somewhat different from others in this book. Instead of building a working implementation, our goal is to construct an executable _model_ that serves as a simple yet precise description of the SOP. Like an implementation, the model can be executed to explore dynamic behaviors of the system; but unlike an implementation, the model omits low-level details that often get in the way of understanding the essential concepts.

To construct this model, we use _Alloy_, a language for modeling and analyzing software design. An Alloy model cannot be executed in the traditional sense of program execution. Instead, a model can be (1) _simulated_ to produce an _instance_, which represents a valid configuration or a trace, and (2) _checked_ to see whether the model satisfies a desired _property_.

The approach we’ll take might be called “agile modeling” because of its similarities to agile programming. We’ll work incrementally, assembling the model bit by bit. Our evolving model will at every point be something that can be executed. We’ll formulate and run tests as we go, so that by the end we'll not only have the model itself but also a collection of properties that it satisfies. 

Despite these similarities, agile modeling differs from agile programming in one key respect. Although we'll be running tests, we actually won't be writing any. Alloy's analyzer generates test cases automatically, and all that needs to be provided is the property to be checked. Needless to say, this saves a lot of trouble (and text). The analyzer actually executes all possible test cases up to a certain size (called a _scope_); this typically means generating all starting states with at most some number of objects, and then choosing operations and arguments to apply up to some number of steps. Because so many tests are executed (typically billions), and because all relationships between state components and all structural shapes are covered (albeit within the scope), this analysis tends to expose bugs more reliably than conventional testing. 

## Alloy Model

### Simplification

Our Alloy model, like any other model, is not intended to be a complete description of the web system, and abstracts away details that we deemed to be of little importance to our discussion of the SOP. In particular, some of the details that are omitted from our model are:
* Low-level network communication between a server and a client (routing, naming server, packet structure, etc.)
* Many aspects of a web browser, including page layout, cookies, DOM events
* Server configuration and interaction with its underlying OS

Each of these details plays a vital role in the functioning of the Internet as a whole, but they are not crucial to understanding the SOP. The ability to omit such details is one of the benefits of a modeling language like Alloy; unlike a program that requires all the nitty-gritty details to be filled in before being executable, an Alloy model can be used to reason about properties of a system even when only a small part of it has been specified. 

### Messages

At its heart, the purpose of the web is to share some resources, so we start by declaring a set of resources:
```
abstract sig Resource {}
```
The keyword “sig” identifies this as an Alloy signature declaration. This introduces a set of resource objects; think of these, just like the objects of a class with no instance variables as blobs that have identity but no contents. The keyword “abstract” means that when we later declare particular subsets of resources every resource will belong to one of those subsets (just as every member of an abstract class must be a member of one of its subclasses). 

Likewise, we introduce a set of endpoints:
```
abstract sig EndPoint { owns: set Resource }
```
also declared as abstract, since we intend to classify endpoints later into browsers and servers and so on. This signature also declares a relation “owns” that maps each endpoint to a set of resources that originate there. As if we had declared an instance variable in a class, we’ll be able to write e.owns for the endpoints owned by an endpoint e. But because owns is actually a mathematical relation, we’ll be able to use relational operators on it. In fact, the dot operator is a relational join, and as well as writing e.owns for the resources owned by endpoint e, we can write owns.r for the set of endpoints that own a resource r (navigating, as it were, backwards through the relation).

Now we declare a set of messages:
```
abstract sig Msg { from, to: EndPoint, payload: set Resource }
```
In this signature, there are three relations: each message has two endpoints, from (the sender) and to (the receiver), and a set of resources if carries as a payload.

Although we’ve hardly written much, it’s not too early to execute our model. The simplest execution command is
```
run {}
```
which tells the analyzer to find any instance satisfying the constraints we’ve written. This might sound silly, but it’s surprising how helpful it can be to be shown some sample scenarios. In this case, we get a scenario with two endpoints, and two messages, one in each direction, and a single resource carried by one message and owned by neither endpoint:

![alt-text](fig-message.png)

This makes us wonder how message payloads are related to ownership. We could require that the payload of every message m is in the set of resources owned by its sender. Running the command
```
run { all m: Msg | m.payload in m.from.owns }
```
now gives us a scenario again with two messages, but this time both are sent and received by a single endpoint, and carry a message owned by that endpoint. We might forbid an endpoint sending a message to itself, or a message carrying a payload to a receiver that already owns that resource. But neither of these constraints seem necessary. In fact the constraint that a message’s payload originate at the sender is not necessary either, since we will surely want to allow a message to be passed along a chain of endpoints.

What we’d like to say, therefore, is that the payload of a message must either originate with the sender or have been received by the sender. We have no notion of time ordering our messages, however, so we introduce it like this:
```
open Stepper[Msg] as step
```
This imports a generic module that applies an ordering to the given type (in this case, Msg). The qualified name step/next, which can be shortened to next when the context is clear, will now be a relation that maps a message to the next one in the order; similarly, nexts maps a message to the ones that follow it in one or more steps; and prev and prevs are defined likewise looking backwards.

Given an endpoint e, to.e (as explained above, for the case of owns), will be the set of messages sent to e. Since m.from is the endpoint a message m is sent from, we can write
```
to.(m.from)
```
for the set of messages that are sent to the sender of m. The subset of these that were sent prior to m can be expressed by intersecting this set with the set of previous messages:
```
to.(m.from) & m.prevs
```
So now we can write our constraint:
```
fact {
  all m: Msg | let ms = to.(m.from) & m.prevs | m.payload in m.from.owns + ms.payload
  }
```
which says that the payload of a message m is either owned by the message’s sender, m.from, or is in the payload of previous messages sent to the sender.

The power of the relational join operator is evident here. Although it’s often used as if it were just a dereferencing dot (as in object oriented languages), it’s actually a more general operator. In the expression ms.payload, the variable ms can refer to a set of zero, one of more messages; the expression denotes the set of all resources that are in the payload of all the previous messages.

[Now execute this?]

### Same Origin Policy

Before we can describe the SOP, the first thing we should do is to define what it means for two pages to have the *same* origin. Two URLs are considered to belong to the same origin if and only if they share the same hostname, protocol, and port:
```
pred sameOrigin[u1, u2 : http/URL] {
  u1.host = u2.host and u1.protocol = u2.protocol and u1.port = u2.port
}
```
The policy itself has two parts, constraining the ability of a script to (1) make DOM AP calls and (2) send HTTP requests. More specifically, the first part of the policy states that a script can only read and write to a DOM inside a frame that comes from the same origin as the script:
```
pred domSOP {
  all d : browser/ReadDOM + browser/WriteDOM | sameOrigin[d.frame.location, d.from.context]
}
```
The second part of the policy prevents a script from sending an HTTP request (i.e., XMLHTTPRequest) to a server unless the script belongs to the same origin as the destination URL: 
```
pred xmlhttpreqSOP {
  all x : browser/XMLHTTPReq | sameOrigin[x.url, x.from.context]
}
```
But why exactly are these restrictions necessary? What would be consequences if today's browsers hadn't enforced to the policy? In the next section, we will see how the Alloy Analyer can be used to answer these types of questions.

### Analyzing the Model

We have already discussed how the Alloy Analyzer can be used to generate valid instances of a system. Another type of analysis is _checking_ whether a model satisfies a property. 

Since our primary concern in this chapter is security, let us consider one property that is desirable in many web applications: _critical resources should only be accessible to trusted modules_. Before stating this property precisely in Alloy, we first designate some subsets of resources and modules to be _critical_ and _trusted_, respectively:
```
sig CriticalResource in message/Resource {} 
sig Trusted in message/EndPoint {}
```
Then, we state our security property as an _assertion_ and _check_ whether it holds true over every instance of the model:
```
assert noResourceLeak {
  all r : CriticalResource, e : EndPoint | r in message/accesses[e] implies e in Trusted
}
check noResourceLeak for 5
```
When executed, the _check_ command instructs the analyzer to explore all possible traces of the system (with at most 5 endpoints, resources, etc.) and look for a _counterexample_ that represents a violation of the property. The analysis is _exhaustive_, and so if there exists a counterexample within the bound, the analyzer is guaranteed to find it. 

In our case, a counterexample, if it exists would consist of a scenario in which a non-trusted script or server accesses a piece of critical resource:

[figure]

### Modeling the SOP

We can now build on top of the basic web model to describe the same-origin policy.

### Bypassing the SOP

[TODO]

### Cross-Origin Resource Sharing (CORS)

[TODO]

### Cross-Document Messaging

Another method for enabling communication between two different origins is a new feature that has been introduced in HTML5 called _cross-document messaging_.

Most browsers implement cross-document messaging using an API function called _postMessage_. In short, postMessage can be used by a script to send data to another script in a different frame, given that the latter has already set up to receive such messages. The key idea is that by setting up mutual agreement, two scripts can safely communicate to each other, even if they are from different origins.

In Alloy, we extend our original browser model with a new type of message:
```
sig PostMessage extends browser/DomAPICall {
    message : Resource, 
    srcOrigin, targetOrigin : URL
}{
	from + to in browser/Script
	payload = message
}
```
The browser, as a security measure, ensures that `targetOrigin`, provided as a field of the API call by the sender, matches the origin of the receiving script. Otherwise, the message could end up in a (potentially malicious) script that the sender did not intended:
```
all m : PostMessage | sop/sameOrigin[m.targetOrigin, m.to.context]
```

But postMessage is not perfect, and a careless use of postMessage can be dangerous! As a guideline, the receiving script is encouraged to check the origin of the message (and discard it if it comes from a dubious origin), but no such check is enforced by default. This means that an attacker's script could easily exploit a lack of an origin check and inject malicious data into the receiving frame, potentially leading to an XSS attack; in fact, a recent study demonstrated that many of the most popular sites on the web suffered from this vulnerability [cite postMessage study paper].

## Conclusion

[TODO]

To fully implement the SOP would have surely required more than 500 lines of code, and probably involve not only a browser but the server as well as a communication network.
