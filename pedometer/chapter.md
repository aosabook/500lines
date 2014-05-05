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

## The Toolchain
* Sinatra web app, using Highcharts to display data.
* This was chosen to be built as a web app because a web app naturally separates the data processing from the presentation.
* Sinatra gives us the ability to demosntrate a fully-functional web app accepting input and presenting output very easily, without worrying about the piping. 
* While this project is not intended to show the separation of concerns present in a well-built web app, using Sinatra allows us to naturally segment those concerns and isolate the data processing from the presentation.
* Using Sinatra and Highcharts requires very little additional code and presents our data nicely, so, well, why not have some fancy charts to really satisfy our data craving?

## The Platform
TODO: Describe basic flow. 

1. Upload data in one of two formats:
	* Combined: Data in what we'll call the **combined** format is user acceleration combined with gravitational acceleration. Data in this format is passed in as x, y, z coordinates, each of which shows combined acceleration in that direction at a point in time.
	* Separated: Data in what we'll call the **separated** format is user acceleration separated from gravitational acceleration. Data in this format is passed in as x, y, z coordinates showing user acceleration is each of the directions, followed by x, y, z coordinates showing gravitational acceleration.
2. Input the following metadata:
	* Sampling rate
	* Actual step count
	* Trial number
	* Method (walking, running, etc.)
	* Gender
	* Height
	* Stride
3. Our program parses the data and outputs:
	* Number of steps taken
	* Distance traveled
	* Time duration
	* Charts representing the data in two different stages of parsing

The meat and potatoes of our program is in step 3, where we parse the input data. 

## Parsing Input Data

The sample data we'll be using here is data collected by an iPhone. Let's look at what our input data will look like in each format. 

### Combined Format
The first, more rudimentary data format we'll accept is in the combined format. Data in the combined format is simply total acceleration in the x, y, z directions, over time. 

$"x1,y1,z1;...xn,yn,zn;"$

Let's look at what this data looks like when plotted. Below is a small portion of data, sampled 100 times per second, of a person walking with an iPhone in a bag on their shoulder.

TODO: Add plot.

### Separated Format

The second data format we'll accept is user acceleration in the x,y,z directions separated from gravitational acceleration in the x,y,z directions, over time:

$"x1_{u},y1_{u},z1_{u}|x1_{g},y1_{g},z1_{g};...xn_{u},yn_{u},zn_{u}|xn_{g},yn_{g},zn_{g};"$

Let's look at what this data looks like when plotted. Below is a small portion of data, sampled 100 times per second, of a person walking with an iPhone in a bag on their shoulder.

TODO: Add 2 plots, one for x,y,z user and one for x,y,z gravity.

### Making Sense of Data

Looking at our plots, we can start to see a pattern, but we don't have enough, yet, to count steps. 

We need to do 3 things to our input data:

1. Parse our text data and extract numerical data.
2. Isolate movement in the direction of gravity to get a single data series resembling a sine wave.
3. Filter our data series to smooth out our sine wave.

These 3 tasks are related, and it makes sense to combine them into one class called a **Parser**. 

### The Parser Class

~~~~~~~
class Parser

  GRAVITY_COEFF = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  
  SMOOTHING_COEFF = {
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }  

  FORMAT_COMBINED  = 'combined'
  FORMAT_SEPARATED = 'separated'

  attr_reader :data, :format, :parsed_data, :dot_product_data, :filtered_data

  def initialize(data)
    @data = data.to_s

    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

  def is_data_combined?
    @format == FORMAT_COMBINED
  end

private

  def split_accl_combined(accl)
    @format = FORMAT_COMBINED
    
    accl = accl.flatten.map { |i| i.split(',').map(&:to_f) }
    split_accl = accl.transpose.map do |total_accl|
      grav = chebyshev_filter(total_accl, GRAVITY_COEFF)
      user = total_accl.zip(grav).map { |a, b| a - b }
      [user, grav]
    end
    split_accl.transpose
  end

  def split_accl_separated(accl)
    @format = FORMAT_SEPARATED
    
    accl = accl.map { |i| i.map { |i| i.split(',').map(&:to_f) } }
    [accl.map {|a| a.first}.transpose, accl.map {|a| a.last}.transpose]
  end

  def parse_raw_data
    accl = @data.split(';').map { |i| i.split('|') }
    
    split_accl = if accl.first.count == 1
      split_accl_combined(accl)
    else
      split_accl_separated(accl)
    end

    user_accl, grav_accl   = split_accl
    user_x, user_y, user_z = user_accl
    grav_x, grav_y, grav_z = grav_accl
    
    @parsed_data = []
    accl.length.times do |i|
      @parsed_data << { x: user_x[i], y: user_y[i], z: user_z[i],
                        xg: grav_x[i], yg: grav_y[i], zg: grav_z[i] }
    end
  rescue
    raise 'Bad Input. Ensure data is properly formatted.'
  end

  def dot_product_parsed_data
    @dot_product_data = @parsed_data.map do |data|
      data[:x] * data[:xg] + data[:y] * data[:yg] + data[:z] * data[:zg]
    end
  end

  def filter_dot_product_data
    @filtered_data = chebyshev_filter(@dot_product_data, SMOOTHING_COEFF)
  end

  def chebyshev_filter(input_data, coefficients)
    output_data = [0,0]
    (2..input_data.length-1).each do |i|
      output_data << coefficients[:alpha][0] * 
                      (input_data[i]    * coefficients[:beta][0] +
                       input_data[i-1]  * coefficients[:beta][1] +
                       input_data[i-2]  * coefficients[:beta][2] -
                       output_data[i-1] * coefficients[:alpha][1] -
                       output_data[i-2] * coefficients[:alpha][2])
    end
    output_data
  end

end
~~~~~~~

Let's start with the initialize method. Our parser class takes string data as input and stores it in the @data instance variable. It then calls three methods in sequence: parse_raw_data, dot_product_parsed_data, and filter_dot_product_data. 

Each method accomplishes one of our three steps above. Let's look at each method individually. 

### Step 1: Parsing text to extract numerical data (parse_raw_data)

The goal of parse_raw_data is to convert string data to a format we can more easily work with, and store it in @parsed_data. The first line splits the string by semicolon into as many arrays as samples taken, and then splits each individual array by the pipe, storing the result in accl.

We determine the input format by the first element of accl, which is an array. 

* accl in the combined format: $[["x1,y1,z1"],...["xn,yn,zn"]]$
* accl in the separated format: $[["x1_{u},y1_{u},z1_{u}", "x1_{g},y1_{g},z1_{g}"],...["xn_{u},yn_{u},zn_{u}", "xn_{g},yn_{g},zn_{g}"]]$

If the array has exactly one element, we know that our input format is combined. Otherwise, our input format is separated. Based on this, we call either split_accl_combined or split_accl_separated. Each of these methods sets the @format instance variable, and generates data in the format below. We store this result in split_accl:

$[[[x1_{u},...xn_{u}], [y1_{u},...yn_{u}], [z1_{u},...zn_{u}]],
[[x1_{g},...xn_{g}], [y1_{g},...yn_{g}], [z1_{g},...zn_{g}]]$

TODO: Dig into the details of split_accl_combined and split_accl_separated. Specifically, chat about how split_accl_combined low pass filters to grab the gravitational data.

The next line splits out the split_accl array into user_accl and grav_accl, which are both arrays of arrays, with user acceleration in the x, y, z directions and gravitational acceleration in the x, y, z directions, respectively. The two lines following split each of user_accl and grav_accl into their x, y, z components:

![](chapter-figures/figure-1.png)

In order to get one data series we can work with, we then create an array of hashes in the format below, and store it in @parsed_data.

$[\lbrace x\colon x1_{u}, y\colon y1_{u}, z\colon z1_{u}, xg\colon x1_{g}, yg\colon y1_{g}, zg\colon z1_{g} \rbrace,...\lbrace x\colon xn_{u}, y\colon yn_{u}, z\colon zn_{u}, xg\colon xn_{g}, yg\colon yn_{g}, zg\colon zn_{g}\rbrace]$

The entire purpose of the parse_raw_data method is to take input data in one of two formats, and output data in this more workable format.

### Step 2: Isolating movement in the direction of gravity

First, a very small amount of liner algebra 101. 

TODO: Short explanation of why the dot product is used to help us isolate movement in the direction of gravity.

Taking the dot product in our Parser class is straightforward. We add a @dot_product_data instance variable, and a method, dot_product_parsed_data, to set that variable. The dot_product_parsed_data method is called immeditely after parse_raw_data in the initializer, and iterates through our @parsed_data hash, calculates the dot product with map, and sets the result to @dot_product_data. 

### Step 3: Filtering our data series

Again, back to the mathematics for some signal processing 101.

TODO: Basics of filtering, Chebyshev filter specifically

Following the pattern from steps 1 and 2, we add another instance variable, @filtered_data, to store the filtered data series, and a method, filter_dot_product_data, that we call from the initializer.

The filter_dot_product_data method initalizes the data series by setting the first two elements to 0, and then iterates through the remaining element indeces in @dot_product_data, applying the Chebyshev filter. 

### Our Parser class in the wild

Our Parser now takes string data in the separated format, converts it into a more useable format, isolates movement in the direction of gravity through the dot product operation, and filters the resulting data series to smooth it out. 

Our parser class is useable on its own as is. An example with combined data:

~~~~~~~
> data = '0.123,-0.123,5;0.456,-0.789,0.111;-0.212,0.001,1;'
> parser = Parser.new(data)

> parser.format
=> 'combined'
> parser.parsed_data
=> [{:x=>0.123, :y=>-0.123, :z=>5.0, :xg=>0, :yg=>0, :zg=>0},
    {:x=>0.456, :y=>-0.789, :z=>0.111, :xg=>0, :yg=>0, :zg=>0},
    {:x=>-0.2120710948533322,
   	 :y=>0.0011468544965549535,
   	 :z=>0.9994625125426089,
   	 :xg=>7.109485333219216e-05,
     :yg=>-0.00014685449655495343,
     :zg=>0.0005374874573911294}]
> parser.dot_product_data
=> [0.0, 0.0, 0.0005219529804999682]
> parser.filtered_data
=> [0, 0, 4.9828746074755684e-05]
~~~~~~~

An example with separated data:

~~~~~~~
> data = '0.028,-0.072,5|0.129,-0.945,-5;0,-0.07,0.06|0.123,-0.947,5;0.2,-1,2|0.1,-0.9,3;'
> parser = Parser.new(data)

> parser.format
=> 'separated'
> parser.parsed_data
=> [{:x=>0.028, :y=>-0.072, :z=>5, :xg=>0.129, :yg=>-0.945, :zg=>-5}, 
	{:x=>0, :y=>-0.07, :z =>0.06, :xg=>0.123, :yg=>-0.947, :zg=>5},
	{:x=>0.2, :y=>-1.0, :z=>2.0, :xg=>0.1, :yg=>-0.9, :zg=>3.0}]
> parser.dot_product_data
=> [-24.928348, 0.36629, 6.92]
> parser.filtered_data
=> [0, 0, -1.7824384769309702]
~~~~~~~

### Things to note
* Ability to pass in either format and the Parser determines it. It's the only class that has to be concerned with it. 
* ...

### Where to improve
* Exception handling in parse_raw_data can be more specific (rather than capturing any error that occurs)

## Pedometer functionality

Our pedometer will measure 3 metrics:

1. Steps taken
2. Distance traveled
3. Time traveled

Let's discuss the infomation we'll need to calculate each of these metrics.

### Steps taken
TODO: above

### Distance traveled
A mobile pedometer app would generally be used by one person. The stride length of that person would be a necessary value to determine distance traveled. The pedometer can ask the user to input their info.

If the user can directly provide their stride length, then we're good to go. If not, and they provide their gender and their height, we can use $0.413 * height$ for a female, and $0.415 * height$ for a male. 

If they only provide their height, we can use $(0.413 + 0.415)/2 * height$, averaging the two multipliers. 

If they only provide their gender, we can use the average of 70 cm for a female, and 78 cm for a male.

Finally, is the user does not wish to provide any information, we can simply take the average of 70 cm and 78 cm and set the stride length to 74 cm.

TODO: Add references for multipliers and averages above.

All of this information is related to the user, so it makes sense to include it in a User class. 

~~~~~~~
class User

  GENDER      = ['male', 'female']
  AVERAGES    = {'female' => 70, 'male' => 78}
  MULTIPLIERS = {'female' => 0.413, 'male' => 0.415}

  attr_reader :gender, :height, :stride

  def initialize(gender = nil, height = nil, stride = nil)
    @gender = gender.to_s.downcase if GENDER.include? gender.to_s.downcase
    @height = height.to_f if height.to_f > 0
    @stride = (stride.to_f > 0) ? stride.to_f : calculate_stride
  end

private

  def calculate_stride
    if gender && height
      MULTIPLIERS[@gender] * height
    elsif height
      height * (MULTIPLIERS.values.reduce(:+) / MULTIPLIERS.size)
    elsif gender
      AVERAGES[gender]
    else
      AVERAGES.values.reduce(:+) / AVERAGES.size
    end
  end

end
~~~~~~~

Things to note:

* Information is optional. The class handles it.
* Magic numbers are defined at the top.
* Basic input data formatting in the initalizer allows for a case insensitive gender parameter, prevents a non-numerical height and stride or a height and stride less than 0. Finally, the stride is calculated through the calculate_stride method unless a valid stride is provided.
* Even when all parameters are provided, the input stride takes precedence. 

### The User class in the wild

The User class is straightforward to use. Below are examples of users created with the least specific to the most specific data:

* Without any parameters
* A gender parameter
* A height parameter
* Gender and height parameters
* A stride parameter
* All parameters

~~~~~~~
> User.new.stride
=> 74
> User.new('Female').stride
=> 70
> User.new(nil, '167.5').stride
=> 69.345
> User.new('male', 191).stride
=> 79.265
> User.new(nil, nil, '80').stride
=> 80.0
> User.new('female', 1, '72').stride
=> 72.0
~~~~~~~

TODO: Introduce User, Device, and Analyzer class at once. Show some working examples through command line of both classes in action. 

## Adding some friendly
* Sinatra layout
* /trials and a basic table with calculations, pulling from the public directory
* ...



    















