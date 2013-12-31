Pedometer
=========

A pedometer implemented in Ruby. 

The pedometer takes accelerometer data from a mobile device, and outputs step count, distance, speed, and other relevant information typically found in native mobile applications. 

While a pedometer application for a mobile device would usually be written natively for the specific platform, this application is written in Ruby, and exposed through a simple API. This illustrates the design choices that are made when writing an application of this sort, without being bound to the specifics of one platform with quickly evolving APIs and native tools. 

Prerequisites
=============
Ruby 2.1.0
gem install sinatra
gem install thin
gem install tilt-jbuilder
gem install rack-test
