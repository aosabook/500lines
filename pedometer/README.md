Pedometer
=========

A pedometer implemented in Ruby. 

The pedometer takes data from a mobile device, and outputs step count, distance, speed, and other relevant information typically found in native mobile applications. 

Input data can come in the two forms below. See public/uploads for an example of each type of data set.
* 1. Combined acceleration in x, y, z.
* 2. User acceleration in x, y, z, and gravitational acceleration in x, y, z.

While a pedometer application for a mobile device would usually be written natively for the specific platform, this application is written in Ruby, and exposed through a simple API. This illustrates the design choices that are made when writing an application of this sort, without being bound to the specifics of one platform with quickly evolving APIs and native tools. 

Prerequisites
=============
* Ruby 2.1.0
* gem install bundler
* bundle install

Running the App
=============
* This app uses Sinatra. Start the web server by running **ruby pedometer.rb** from the **pedometer/code** directory. 
* Navigate to **localhost:4567/uploads**. 
* You'll see a few example uploads. Both of these uploads are from the same walk, but the first parses data in format 1 above (combined acceleration), and the second parses data in format 2 (user acceleration and gravitational acceleration). 
* The form on the right allows you to upload a data set, parse it, and save it to the file system. 
* Clicking on the **Detail** link shows more information, as well as the first 1000 points in the data series produced at the two main stages of the step count calculation. 
