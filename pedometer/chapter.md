# A Basic Pedometer
TODO: Intro

## Simplifications
* Ruby instead of native for mobile (Objective-C or Java)
  * A pedometer is a common app built for mobile devices that have hardware/software built in to measure acceleration and gravity. 
  * If the mobile device is an iPhone or Android, the pedometer would commonly be written natively for the platform in Objective-C or Java, respectively. 
  * Java is verbose, and Objective-C is both verbose and difficult on the eyes for a developer not familiar with it.
  * Additionaly, native mobile APIs are quickly evolving, and code that may be accurate now may not be in several years.
  * Our basic pedometer is written in Ruby for two reasons: to keep the complexities of the language out of the way and allow us to focus on architecture, and to ensure that code for the specifics of native mobile platforms as they are today is not confused with code for data processing and presentation.
* Batch processing instead of real-time
  * A pedometer would rarely be written as a batch processing problem analyzed by a web application, but it has been done this way for the purposes of simplification.
  * The concepts behind our basic pedometer can be extended and directly applied as mobile applications analyzing data in real-time.
  * Data has been collected from an iPhone in two formats, and is being abalzed by our web application in Ruby.
* Error detection can be enhanced
  * Currently we're not counting steps that are too close together. 
  * One enhancement would be to discount any steps if there are too many false steps. 
  * TODO: More error detection suggestions.
* Many ways to analyze data to count steps
  * There are many methods present to analyze movement data and count steps. Some are more accurate than others in specific instances, for example, day-to-day tracking vs. step counting during a jog.
  * This is just one of many ways. 

## The Platform
* Sinatra web app, using Highcharts to display data.
* This was chosen to be built as a web app because a web app naturally separates the data processing from the presentation.
* Sinatra gives us the ability to demosntrate a fully-functional web app accepting input and presenting output very easily, without worrying about the piping. 
* While this project is not intended to show the separation of concerns present in a well-built web app, using Sinatra allows us to naturally segment those concerns and isolate the data processing from the presentation.
* Using Sinatra and Highcharts requires very little additional code and presents our data nicely, so, well, why not have some fancy charts to really satisfy our data craving?
















