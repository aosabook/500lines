# Exploring the Same Origin Policy with Alloy

## Introduction

Like the other chapters in this book, ours explores a software feature, using code to help convey how it works. The feature we’ll explore is the Same Origin Policy (SOP), implemented in all browsers, and augmented by mechanisms such as Cross Origin Resource Sharing (CORS) that are implemented in servers. Unlike the other chapters, however, ours is not focused on the implementation, but on the specification. We’re interested in the idea of SOP: what it does, and why.

For this purpose, our code is written not in a standard programming language but in Alloy, a software modeling language. The approach we’ll take might be called “agile modeling” because of its similarities to agile programming. First, we’ll work incrementally, assembling the model bit by bit. Second, our evolving model will at every point be something that can be executed. Third, we’ll run tests as we go, and will formulate properties to be checked (which, like assertions in code, will become an important part of the final model). 

At the same time, there are some important differences. Our model is not an implementation, and could not be used directly in a running system. The “execution” that’s performed is actually a form of constraint solving to find sample behaviors. Most importantly, although we’ll run tests, we’ll never actually write any. Instead we’ll present Alloy’s tool, the Alloy Analyzer, with a property to be checked, and the tool will automatically generate a suite of tests, each corresponding to a possible execution, and will check the property on each. The tool will display the first execution it finds that violates the property (if any).

Using Alloy rather than a conventional programming language not only eliminates the trouble of writing test cases. It also means that many more tests are executed. It’s not possible in general to run all tests (since there’s an infinite number of those). But what Alloy can do is run all tests up to a certain size (chosen by the user). As you’ll see, the basic data structure in Alloy is a relation (which is like a hashmap, except that each key can map to an entire set of values, or like a two-column database table). If the user selects a size of 3, say, that would allow all relations with up to 3 keys and 3 values. Since there are 9 possible links from keys to values, there are 2^9 candidate relations. So you only need a handful of such structures in the state for there to be a billion or more starting states. In practice, the huge volume of tests — even just considering “small tests” — makes this far more effective than traditional testing. If the property being checked doesn’t hold, it’s very unlikely that all these tests will pass successfully.

Eliminating the writing of test cases and covering a huge space of tests automatically are the main reasons for preferring Alloy to a programming language. Another is that models are usually much more succinct than programs; this is because the modeling language allows you to express the behavior just by listing its properties, without having to write the detailed code that would make those properties true. More subtly, because the model is just a collection of logical properties, it can be structured as separate submodels, each of which describes the behavior partially. If you’re familiar with aspect-oriented programming, you might want to think of these submodels as cross-cutting concerns. This suits the agile approach well, since the model can be grown by adding new parts without needing to modify old ones. 

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

[… figure]

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
The SOPitself has two parts, constraining the ability of a script to (1) make DOM AP calls and (2) send HTTP requests. More specifically, the first part of the policy states that a script can only read and write to a DOM inside a frame that comes from the same origin as the script:
```
pred domSOP {
  all d : browser/ReadDOM + browser/WriteDOM | sameOrigin[d.frame.location, d.from.context]
}
```
The second part of the policy prevents a script from sending an HTTP request (to be more specific, XMLHTTPRequest) to a server unless the script belongs to the same origin as the desintation URL: 
```
pred xmlhttpreqSOP {
  all x : browser/XMLHTTPReq | sameOrigin[x.url, x.from.context]
}
```
But why exactly are these restrictions necessary? What would be consequences if today's browsers hadn't adhered to the policy? 

### Analyzing the Model

We have already discussed how the Alloy Analyzer can be used to generate valid instances of a system. Another type of analysis is _checking_ whether a model satisfies a property. 

Since our primary concern in this chapter is security, let us consider one property that is desirable in many web applications: _critical resources should never be accessible to malicious modules_. Before stating this property precisely in Alloy, we first designate some subsets of resources and modules to be _critical_ and _malicious_, respectively. Furthermore, we extend the  **Module** signature with a set of resources that a module accesses: 
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
When executed, the _check_ command instructs the analyzer to look for a _counterexample_ -- a system trace that represents a violation of the property. In our case, a counterexample, if it exists would consist of a scenario in which a malicious script or server accesses a piece of critical resource.

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
