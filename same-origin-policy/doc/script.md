Script
------

Next, we will build on the HTTP and browser models to introduce *client-side scripts*, which represent a piece of code (typically in Javascript) executing inside a browser document (`context`). 
```
abstract sig Script extends Client { context : Document }
```
A script is a dynamic entity that can perform two different types of actions: (1) it can make HTTP requests (i.e., Ajax requests) and (2) perform browser operations to manipulate the content and properties of a document. The flexibility of client-side scripts is one of the main catalysts behind the rapid development of Web 2.0, but it's also the reason why the SOP was created in the first place. Without the policy, scripts would be able to send arbitrary requests to servers, or freely modify the documents inside the browser---which would be bad news if one or more of the scripts turned out to be malicious! 

A script can communicate to a server by sending an `XMLHttpRequest`:
```
sig XMLHttpRequest extends HttpRequest {}{
  from in Script
  noBrowserChange[before, after] and noDocumentChange[before, after]
}
```
An `XMLHttpRequest` can be used by a script to send/receive resources to/from a server, but unlike `BrowserHttpRequest`, it does not immediately result in creation of a new page or other changes to the browser and its documents. To say that a call does not modify the states of the system, we use predicates `noBrowserChange` and `noDocumentChange`:
```
pred noBrowserChange[before, after : Time] {
  documents.after = documents.before and cookies.after = cookies.before  
}
pred noDocumentChange[before, after : Time] {
  content.after = content.before and domain.after = domain.before  
}
```
What kind of actions can a script perform on documents? First, we introduce a generic notion of *browser operations* to represent a set of browser API functions that can be invoked by a script:
```
abstract sig BrowserOp extends Call { doc : Document }{
  from in Script and to in Browser
  doc + from.context in to.documents.before
  noBrowserChange[before, after]
}
```
Field `doc` refers to the document that will be accessed or manipulated by this call. The second constraint in the signature facts says that both `doc` and the document in which the script executes (`from.context`) must be documents that currently exist inside the browser. Finally, a `BrowserOp` may modify the state of a document, but not the set of documents or cookies* that are stored in the browser.

(*: actually, cookies can be associated with a document and modified using a browser API, but we will omit this detail for now.)

First, it can use the browser API to *read* various parts of a document (i.e., these parts are also called DOM elements):
```
sig ReadDOM extends BrowserOp { result : Resource }{
  result = doc.content.before
  noDocumentChange[before, after]
}
```
