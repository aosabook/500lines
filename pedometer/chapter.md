
# A Perfect World

Many software engineers reflecting on their training will remember having the pleasure of living in a very perfect world. We were taught to solve discrete problems, with defined parameters, in an ideal domain. 

Then, we were thrown into the real world, with all of its complexities and challenges. It's messy, which makes it all the more exciting. When you can solve a real-life problem, with all of its quirks, you can build software that really helps people, every day. 

In this chapter, we'll examine a problem that looks straightforward on the surface, and gets tangled very quickly when the real world, and real people, are thrown into it. 

Let's roll up our sleeves, and prepare to untangle.

# A Basic Pedometer

The rise of the mobile device brought with it a trend to collect more and more data on our daily lives. One type of data many people today collect is the number of steps they've taken over a period of time. This data can be used for health tracking, training for sporting events, or, for those of us obsessed with collecting and analyzing data, just for kicks. Steps can be counted using a pedometer, which often uses data from a hardware accelerometer as input.

## What's an Accelerometer, You Ask?

An accelerometer is a piece of hardware that measures acceleration in the x, y, and z directions. In today's mobile world, many people carry an accelerometer with them wherever they go, as it's built into almost all smartphones currently on the market. The x, y, and z directions are relative to the phone.

An accelerometer returns a **signal** in 3-dimensional space. A signal is a set of data points recorded over time. Each component of the signal is a time series representing acceleration in one of the x, y, or z directions. Each point in a time series is the acceleration in that direction at a specific point in time. Acceleration is measured in units of g-force. One *g* is equal to $9.8 m/s^2$, which is the gravitational acceleration on Earth.

The diagram below shows an example acceleration signal from an accelerometer with the three time series.

![](chapter-figures/acceleration-total.png)\ 

The **sampling rate** of the accelerometer, which can often be calibrated, determines the number of measurements per second. For instance, an accelerometer with a sampling rate of 100 returns 100 data points for each x, y, and z time series each second.

## Let's Talk About a Walk

When a person walks, they bounce slightly with each step. Just watch the top of a person's head as they walk away from you. Their head, torso, and hips are synchronized in a smooth bouncing motion. While people don't bounce very far, only one or two centimeters, it is one of the clearest, most constant, and most recognizable parts of a person's walking acceleration signal.

A person bounces up and down, in the vertical direction, with each step. If you are walking on Earth, or another big ball of mass floating in space, the bounce is conveniently in the same direction as gravity. 

**We are going to count steps by using the accelerometer to count bounces up and down. Because the phone can rotate in any direction, we will take advantage of gravity to know which direction down is. A pedometer can count steps by counting the number of bounces in the direction of gravity.**

Let's look at a person walking with a smartphone containing an accelerometer, in his or her shirt pocket, as depicted below.

![](chapter-figures/walk-1.png)\

For the sake of simplicity, we'll assume that the stick person:

* is walking in the z direction;
* bounces with each step in the y direction;
* maintains the phone in the same orientation throughout the entire walk;

In our perfect world, acceleration from step bounces will form a perfect sine wave in the y direction. Each peak in the sine wave is exactly one step. Step counting becomes a matter of counting these perfect peaks. 

Ah, the joys of a perfect world, which we only ever experience in texts like this. Don't fret, things are about to get a little messier, and a lot more exciting. Let's add a little more reality to our world. 

## Even Perfect Worlds Have Fundamental Forces of Nature

The force of gravity causes an acceleration in the direction of gravity, which we refer to as gravitational acceleration. This acceleration is unique because it is always present, and is always constant at $9.8 m/s^2$. 

Suppose a smartphone is lying on a table screen-size up. In this orientation, our coordinate system is such that the negative z direction is the one in direction of gravity. Gravity will **pull** our phone in the negative z direction, so our accelerometer, **even when perfectly still**, will record an acceleration of $9.8 m/s^2$ in the negative z direction. Real accelerometer data from our phone in this orientation looks like the graph below. 

![](chapter-figures/acceleration-total-phone-still.png)\

Note that the $x(t)$ and $y(t)$ remain constant at 0, while $z(t)$ is constant at $-1g$. Our accelerometer records all acceleration, including gravitational acceleration.

Each component of the acceleration signal measures the **total acceleration** in that direction. Total acceleration is a result of **user acceleration** and **gravitational acceleration**. 

User acceleration is the acceleration of the device due to the movement of the user, and is constant at 0 when the phone is perfectly still. However, when the user is moving with the device, user acceleration is rarely constant, since it's difficult for a person to move with a constant acceleration. 

Our total acceleration is the sum of user acceleration and gravitational acceleration. 

![](chapter-figures/component-signals-2.png)\

To count steps, we're interested in the bounces created by the user in the direction of gravity. That means we're interested in isolating the 1-dimensional time series **user acceleration in the direction of gravity** from our 3-dimensional acceleration signal. 

When our stick person is walking with step bounces in the y direction, gravitational acceleration is zero in $x(t)$ and $z(t)$ and constant at $9.8 m/s^2$ in $y(t)$, so $x(t)$ and $z(t)$ fluctuate around 0 while $y(t)$ fluctuates around $-1g$. Note the obvious peaks in $y(t)$. Those are due to step bounces! Looking at the user acceleration plots next, we notice that when we remove gravitational acceleration from the time series, all three time series fluctuate around 0. In our last plot, gravitational acceleration, $y_{g}(t)$ is near-constant at $-1g$, $x_{g}(t)$ and $z_{g}(t)$ are near-constant at 0.

So, in our simple case, the 1-dimensional user acceleration in the direction of gravity time series we're interested in is $y_{u}(t)$. Although $y_{u}(t)$ isn't as smooth as our perfect sine wave, we can identify the peaks, and use those peaks to count steps. So far, so good. Now, let's add a little more reality to our world. 

## People Are Complicated Creatures

Let's see what happens when we introduce real people into the mix. What if a person carries the phone in a bag on their shoulder, with the phone in a more wonky position? To make matters worse, what if the phone rotates in the bag part way through the walk?

![](chapter-figures/walk-2.png)\

Yikes. Now all three of our components have a non-zero gravitational acceleration, so the user acceleration in the direction of gravity is now split amongst all three x, y, and z time series. To determine user acceleration in the direction of gravity, we have to first determine which direction gravity is in. To do this, we have to split total acceleration in each of the three time series into a user acceleration time series and a gravitational acceleration time series. 

![](chapter-figures/component-signals-3.png)\

Then, we can isolate the portion of user acceleration in each component that is in the direction of gravity, resulting in just the user acceleration in the direction of gravity time series. 

Let's define this as two steps below:

1. Splitting total acceleration into user acceleration and gravitational acceleration. 
2. Isolating user acceleration in the direction of gravity. 

Let's look at each problem separately, and put on our mathematician hats. 

## 1. Splitting Total Acceleration Into User Acceleration and Gravitational Acceleration

We can use a tool called a **filter** to split our total acceleration time series into a user acceleration time series and a gravitational acceleration time series.

### Low-pass and High-pass Filters
A **filter** is a tool used in signal processing to remove an unwanted component from a signal. 

A **low-pass filter** allows low-frequency signals through, while attenuating signals higher than a set threshold. Conversely, a **high-pass filter** allows high-frequency signals through, while attenuating signals below a set threshold. Using music as an analogy, a low-pass filter can be used to get rid of the treble, while and a high-pass filter can be used to get rid of the bass. 

In our situation, the frequency, measured in Hz, indicates how quickly the acceleration is changing. A constant acceleration has a frequency of 0 Hz, while a non-constant acceleration has a non-zero frequency. This means that our constant gravitational acceleration is a 0 Hz signal, while user acceleration is not. 

For each component, we can pass total acceleration through a low-pass filter, and we'll be left with just the gravitational acceleration time series. Then, we can subtract gravitational acceleration from total acceleration, and we'll have the user acceleration time series. 

![](chapter-figures/low-pass-filter-a.png)\ 

There are numerous varieties of filters. The one we'll use is called a Chebyshev filter. We've chosen a Chebyshev filter because it has a steep cutoff, which means that it very quickly attenuates frequencies beyond our chosen threshold.

The Chebyshev filter we've chosen is implemented using the formula $output_{i} = \alpha_{0} * (input_{i} * \beta_{0} + input_{i-1} * \beta_{1} + input_{i-2} * \beta_{2} - output_{i-1} * \alpha_{1} - output_{i-2} * \alpha_{2})$. 

The design of digital filters is outside of the scope of this chapter, but a very short teaser discussion is warranted. It's a well-studied, fascinating topic, with numerous practical applications. A digital filter can be designed to cancel any frequency or range of frequencies desired. The $\alpha$ and $\beta$ values in the formula are coefficients, set based on the cutoff frequency, and the range of frequencies we want to preserve. 

We want to cancel all frequencies except for our constant gravitational acceleration, so we've chosen coefficients that attenuate frequencies higher than 0.2 Hz. Notice that we've set our threshold slightly higher than 0 Hz. While gravity does create a true 0 Hz acceleration, our real, imperfect world has real, imperfect accelerometers, so we're allowing for a slight margin of error in measurement. 

### Implementing a Low-pass Filter

Let's work through a low-pass filter implementation using our earlier example. We'll split: 

* $x(t)$ into $x_{g}(t)$ and $x_{u}(t)$, 
* $y(t)$ into $y_{g}(t)$ and $y_{u}(t)$,
* $z(t)$ into $z_{g}(t)$ and $z_{u}(t)$.

We'll initialize the first two values of gravitational acceleration to 0, so that the formula has initial value to work with.

$x_{g}(0) = x_{g}(1) = y_{g}(0) = y_{g}(1) = z_{g}(0) = z_{g}(1) = 0$\

Then, we'll implement the filter formula for each time series.

$x_{g}(t) = \alpha_{0} * (x(t) * \beta_{0} + x(t-1) * \beta_{1} + x(t-2) * \beta_{2} - x_{g}(t-1) * \alpha_{1} - x_{g}(t-2) * \alpha_{2})$\

$y_{g}(t) = \alpha_{0} * (y(t) * \beta_{0} + y(t-1) * \beta_{1} + y(t-2) * \beta_{2} - y_{g}(t-1) * \alpha_{1} - y_{g}(t-2) * \alpha_{2})$\

$z_{g}(t) = \alpha_{0} * (z(t) * \beta_{0} + z(t-1) * \beta_{1} + z(t-2) * \beta_{2} - z_{g}(t-1) * \alpha_{1} - z_{g}(t-2) * \alpha_{2})$\

The resulting time series after low-pass filtering are below. 

![](chapter-figures/acceleration-gravitational.png)\ 

$x_{g}(t)$ and $z_{g}(t)$ hover around 0, and $y_{g}(t)$ very quickly drops to $-1g$. The initial 0 value in $y_{g}(t)$ is from the initialization of the formula.

Now, to receive user acceleration, we can subtract gravitational acceleration from our total acceleration:

$x_{u}(t) = x(t) - x_{g}(t)$\
$y_{u}(t) = y(t) - y_{g}(t)$\
$z_{u}(t) = z(t) - z_{g}(t)$

When we do that, we receive the time series below:

![](chapter-figures/acceleration-user.png)\ 

We've successfully split our total acceleration into user acceleration and gravitational acceleration!

## 2. Isolating User Acceleration in the Direction of Gravity

$x_{u}(t)$, $y_{u}(t)$, and $z_{u}(t)$ include all movements of the user, not just movements in the direction of gravity. Our goal here is to end up with a 1-dimensional time series representing **user acceleration in the direction of gravity**. This time series will include portions of user acceleration in each of our directions.

Let's get to it. First, some liner algebra 101. Don't take that mathematician hat off just yet!

### The Dot Product

When working with coordinates, you won't get very far before being introduced to the **dot product**, one of the fundamental tools used in comparing the magnitude and direction of x, y, z coordinates.

The dot product will take us from 3-dimensional space to 1-dimensional space. 
When we take the dot product of the time series user acceleration and gravitational acceleration, both of which are in 3-dimensional space, we'll be left with a single time series in 1-dimensional space representing the portion of user acceleration in the direction of gravity. We'll arbitrarily call this new time series $a(t)$, because, well, every important time series deserves a name. 

![](chapter-figures/dot-product-explanation.png)\ 

### Implementing the Dot Product
We can implement the dot product for our earlier example using the formula

$a(t) = x_{u}(t)x_{g}(t) + y_{u}(t)y_{g}(t) + z_{u}(t)z_{g}(t)$,

leaving us with $a(t)$, in 1-dimensional space. 

![](chapter-figures/acceleration-dotproduct.png)\ 

We can now visually pick out where the steps are. The dot product is very powerful, yet beautifully simple. 

## Solutions in the Real World 

We saw how quickly our seemingly simple problem turned more complex when we threw in the challenges of the real world and real people. However, we're getting a lot closer to counting steps, and we can see how $a(t)$ is starting to resemble our ideal sine wave. But, only "kinda, sorta" starting to. We still need to make our messy $a(t)$ time series smoother. There are four main issues with $a(t)$ in its current state. Let's examine each one.

![](chapter-figures/jumpy-slow-short-bumpy.png)\ 

### 1. Jumpy Peaks

$a(t)$ is very "jumpy", because a phone can jiggle with each step, adding a high-frequency component to our time series. By studying numerous data sets, we've determined that a step acceleration is at maximum 5 Hz. We can again use a low-pass Chebyshev filter, picking $\alpha$ and $\beta$ to attenuate all signals above 5 Hz, to remove the "jumpiness".

### 2. Slow Peaks

With a sampling rate of 100, the slow peak displayed in $a(t)$ spans 1.5 seconds, which is too slow to be a step. In studying enough samples of data, we've determined that the slowest step we can take is at a 1 Hz frequency. Slower accelerations are due to a low-frequency component, that we can again remove using a high-pass Chebyshev filter, setting $\alpha$ and $\beta$ to cancel all signals below 1 Hz. 

### 3. Short Peaks

As a person is using an app or making a call, the accelerometer registers small movements in the direction of gravity, presenting themselves as short peaks in our time series. We can eliminate these short peaks by setting a minimum threshold, and counting a step every time $a(t)$ crosses that threshold in the positive direction.

### 4. Bumpy Peaks

Our pedometer should accommodate many people with different walks, so we set minimum and maximum step frequencies based on a large sample size of people and walks. This means that we may sometimes filter slightly too much or too little. While we'll often have fairly smooth peaks, we can, once in a while, get a "bumpier" peak. The diagram above zooms in on one such peak. 

When bumpiness occurs at our threshold, we can mistakenly count too many steps for one peak. We can use a method called **hysteresis** to address this. Hysteresis refers to the dependence of an output on past inputs. We can count threshold crossings in the positive direction, as well as 0 crossings in the negative direction. Then, we only count steps where a threshold crossing occurs after a 0 crossing, ensuring we count each step only once. 

### Peaks That Are Juuuust Right

![](chapter-figures/acceleration-filtered.png)\ 

In accounting for these four scenarios, we've managed to bring our messy $a(t)$ fairly close to our ideal sine wave, allowing us to count steps. 

## Recap

The problem, at first glance, looked straightforward. However, the real world and real people threw a few curve balls our way. Let's recap how we solved the problem: 

1. We started with total acceleration, $(x(t), y(t), z(t))$.
2. We used a low-pass filter to split total acceleration into user acceleration, $(x_{u}(t), y_{u}(t), z_{u}(t))$, and gravitational acceleration, $(x_{g}(t), y_{g}(t), z_{g}(t))$.
3. We took the dot product of $(x_{u}(t), y_{u}(t), z_{u}(t))$ and $(x_{g}(t), y_{g}(t), z_{g}(t))$ to obtain the user acceleration in the direction of gravity, $a(t)$. 
4. We used a low-pass filter again to remove the high-frequency component of $a(t)$, removing noise.
5. We used a high-pass filter to cancel the low-frequency component of $a(t)$, removing slow peaks.
6. We set a threshold to ignore short peaks.
7. We used hysteresis to avoid double-counting steps with bumpy peaks.

As software developers in a training or academic setting, we may have been presented with a perfect signal and asked to write code to count the steps in that signal. While that may have been an interesting coding challenge, it wouldn't have been something we could apply in a live situation. We saw that in a reality with gravity and people thrown into the mix, the problem was a little more complex. We used mathematical tools to address the complexities, and were able to solve a real-world problem. It's time to translate our solution into code. 

# Diving Into Code

Our goal for this chapter is to create a web application in Ruby that accepts accelerometer data, parses, processes, and analyzes it, and returns the number of steps taken, the distance traveled, and the elapsed time. 

# Preliminary Work

Our solution requires us to filter our time series several times. Rather than peppering filtering code throughout our program, it makes sense to create a class that takes care of the filtering, and if we ever need to enhance or modify it, we'll only ever need to change that one class. This strategy is called *separation of concerns*, a commonly-used design principle which promotes splitting a program into numerous distinct pieces, where every piece has one primary concern. It's a beautiful way to write clean, maintainable code that's easily extensible. We'll revisit this idea several times throughout the chapter.

Let's dive into the filtering code, contained in, logically, a `Filter` class.

~~~~~~~
class Filter

  COEFFICIENTS_LOW_0_HZ = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  COEFFICIENTS_LOW_5_HZ = { # Direct form I, Chebyshev II, type = low-pass, Astop = 2, Fstop = 5, Fs = 100, Direct Form I
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }
  COEFFICIENTS_HIGH_1_HZ = { # Direct form I, Chebyshev II, type = high-pass, Fs = 100, Fstop = 0.5, Astop = 20, order = 2, 
    alpha: [1, -1.905384612118461, 0.910092542787947], 
    beta:  [0.953986986993339, -1.907503180919730, 0.953986986993339]
  }

  def self.low_0_hz(data)
    chebyshev_filter(data, COEFFICIENTS_LOW_0_HZ)
  end

  def self.low_5_hz(data)
    chebyshev_filter(data, COEFFICIENTS_LOW_5_HZ)
  end

  def self.high_1_hz(data)
    chebyshev_filter(data, COEFFICIENTS_HIGH_1_HZ)
  end

private

  def self.chebyshev_filter(data, coefficients)
    filtered_data = [0,0]
    (2..data.length-1).each do |i|
      filtered_data << coefficients[:alpha][0] * 
                      (data[i]            * coefficients[:beta][0] +
                       data[i-1]          * coefficients[:beta][1] +
                       data[i-2]          * coefficients[:beta][2] -
                       filtered_data[i-1] * coefficients[:alpha][1] -
                       filtered_data[i-2] * coefficients[:alpha][2])
    end
    filtered_data
  end

end
~~~~~~~

Anytime our program needs to filter a time series, we can call one of the class methods in `Filter` with the data we need filtered: 

* `low_0_hz` is used to low-pass filter signals near 0 Hz
* `low_5_hz` is used to low-pass filter signals at or below 5 Hz
* `high_1_hz` is used to high-pass filter signals above 1 Hz

Each class method calls `chebyshev_filter`, which implements the Chebyshev filter and returns the result. If we wish to add more filters in the future, we only need to change this one class. 

One important thing to note is that all "magic" numbers are defined at the top. This makes our class easier to read and understand. The formula in `chebyshev_filter` is much more obvious using the `coefficients` parameter than it would have been with numerical values directly inserted.

# The Pipeline

TODO: Add a little note on "the pipeline"...

# Parsing

Our input data is coming from mobile devices such as Android phones and iPhones. Most mobile phones on the market today have accelerometers built in, that are able to record total acceleration. Let's call the input data format that records total acceleration the *combined format*. Many, but not all, devices can also record user acceleration and gravitational acceleration separately. Let's call this format the *separated format*. A device that has the ability to return data in the separated format necessarily has the ability to return data in the combined format. However, the inverse is not always true. Some devices can only record data in the combined format. Input data in the combined format will need to be passed through a low-pass filter to turn it into the separated format. 

We want our program to handle all mobile devices on the market with accelerometers, so we'll need to accept data in both formats. Let's look at the two formats we'll be accepting individually.

### Combined Format

Data in the combined format is total acceleration in the x, y, z directions, over time. x, y, and z values will be separated by a comma, and samples per unit time will be separated by a semi-colon.

$"x1,y1,z1; ... xn,yn,zn;"$

### Separated Format

The separated format returns user acceleration in the x, y, z directions as well as gravitational acceleration in the x, y, z directions, over time. User acceleration values will be separated from gravitational acceleration values by a pipe.

$"x_{u}1,y_{u}1,z_{u}1|x_{g}1,y_{g}1,z_{g}1; ... x_{u}n,y_{u}n,z_{u}n|x_{g}n,y_{g}n,z_{g}n;"$

## I Got Multiple Input Formats But a Standard Ain't One

Dealing with multiple input formats is a common programming problem. If we want our entire program to work with both formats, every single piece of code dealing with the data would need to know how to handle both formats. This can become very messy, very quickly, especially if a third (or a fourth, or a fifth, or a hundredth) input format is added.

### Standard Format

The cleanest way for us to deal with this is to take our two input formats and determine a standard format to fit them both into as soon as possible, allowing the rest of the program to work with this new standard format. We'll write a parser that allows us to take our two known input formats and convert them to a standard output format. In the future, if we ever have to add another input format, the only code we'll have to touch is this parser. Here with see another example of separation of concerns.

![](chapter-figures/input-data-workflow-1.png)\

We'll need to work with user acceleration and gravitational acceleration separately in order to follow our solution, so our standard format will need to split out the two accelerations. This means that if our data is in the combined format, we'll need to first pass it through a low-pass filter to convert it to the standard format. Take note of the standard format:

![](chapter-figures/standard-format.png)\

Our standard format allows us to store a time series, as each element represents acceleration at a point in time. We've defined it as an array of arrays of arrays. Let's peel back that onion. 

* The first array is just a wrapper to hold the all of the data.
* The second set of arrays contains one array per data sample taken. If our sampling rate is 100 and we sample data for 10 seconds, we'll have $10 * 100$, or 1000, arrays in this second set. 
* The third set of arrays is the pair of arrays enclosed within the second set. They both contain acceleration data in the x, y, and z directions; the first representing user acceleration and the second gravitational acceleration.

Let's separate concerns once more, and create a `Parser` class to handle the parsing.

~~~~~~~
require_relative 'filter'

class Parser

  attr_reader :parsed_data

  def self.run(data)
    parser = self.new(data)
    parser.parse
    parser
  end

  def initialize(data)
    @data = data
  end

  def parse
    @parsed_data = @data.to_s.split(';').map { |x| x.split('|') }
                   .map { |x| x.map { |x| x.split(',').map(&:to_f) } }

    unless @parsed_data.map { |x| x.map(&:length).uniq }.uniq == [[3]]
      raise 'Bad Input. Ensure data is properly formatted.'
    end

    if @parsed_data.first.count == 1
      filtered_accl = @parsed_data.map(&:flatten).transpose.map do |total_accl|
        grav = Filter.low_0_hz(total_accl)
        user = total_accl.zip(grav).map { |a, b| a - b }
        [user, grav]
      end

      @parsed_data = @parsed_data.length.times.map do |i|
        user = filtered_accl.map(&:first).map { |elem| elem[i] }
        grav = filtered_accl.map(&:last).map { |elem| elem[i] }
        [user, grav]
      end
    end
  end

end
~~~~~~~

`Parser` has a class-level `run` method as well as an initializer. This is a pattern we'll use several times, so it's worth a discussion. Initializers should generally be used for setting up an object, and shouldn't do a lot of work. `Parser`'s initializer simply takes `data` in the combined or separated format and stores it in the instance variable `@data`. The `parse` instance method uses `@data` internally, and does the heavy lifting of parsing and setting the result in the standard format to `@parsed_data`. In our case, we'll never need to instantiate a `Parser` instance without having to immediately call `parse`. Therefore, we add a convenient class-level `run` method, that instantiates an instance of `Parser`, calls `parse` on it, and returns the instance of the object. We can now pass our input data to `run`, knowing we'll receive an instance of `Parser` with `@parsed_data` already set.

Let's take a look at our hard-working `parse` method. The first step in the process is to take string data and convert it to numerical data, giving us an array of arrays of arrays. Sound familiar? The next thing we do is ensure that the format is as expected. Unless we have exactly three elements per the innermost arrays, we throw an exception. Otherwise, we continue on.

Note the differences in `@parsed_data` between the two formats at this stage:

* `@parsed_data` in the *combined format* contains arrays with exactly **one** array: $[[[x1, y1, z1]], ... [[xn, yn, zn]]$
* `@parsed_data` in the *separated format* contains arrays with exactly **two** arrays: $[[[x_{u}1,y_{u}1,z_{u}1], [x_{g}1,y_{g}1,z_{g}1]], ... [[x_{u}n,y_{u}n,z_{u}n], [x_{g}n,y_{g}n,z_{g}n]]]$

The separated format is already in our desired standard format after this operation. Amazing. However, if the data is combined (or, equivalently, has exactly one array where the separated format would have two), then we proceed with two loops. The first loop splits total acceleration into gravitational and user, using `Filter` with a `:low_0_hz` type, and the second loop reorganizes the data into the standard format.

`parse` leaves us with `@parsed_data` holding data in the standard format, regardless of whether we started off with combined or separated data. What a relief!

As our program becomes more sophisticated, one area for improvement is to make our users' lives easier by throwing exceptions with more specific messages than "Bad Input. Ensure data is properly formatted.", allowing them to more quickly track down common input formatting problems. 

# Processing

Based on the solution we defined, we'll need our code to do a few things to our parsed data before we can count steps:

1. Isolate movement in the direction of gravity using the dot product.
2. Remove jumpy (high-frequency) and slow (low-frequency) peaks with a low-pass filter followed by a high-pass filter.

The removal of short and bumpy peaks can be handled during step counting. 

Now that we have our data in a standard format, we can process it to get in into a state where we can analyze it to count steps. 

![](chapter-figures/input-data-workflow-2.png)\

The purpose of processing is to take our data in the standard format and incrementally clean it up to get it to a state as close as possible to our ideal side wave. Our two processing operations, taking the dot product and filtering, are quite distinct, but both are intended to process our data, so we'll create one class, called a **Processor**. 

~~~~~~~
require_relative 'filter'

class Processor

  attr_reader :dot_product_data, :filtered_data

  def self.run(data)
    processor = self.new(data)
    processor.dot_product
    processor.filter
    processor
  end

  def initialize(data)
    @data = data
  end

  def dot_product
    @dot_product_data = @data.map do |x|
      x[0][0] * x[1][0] + x[0][1] * x[1][1] + x[0][2] * x[1][2]
    end
  end

  def filter
    @filtered_data = Filter.low_5_hz(@dot_product_data)
    @filtered_data = Filter.high_1_hz(@filtered_data)
  end

end
~~~~~~~

Again, we see the `run` and `initialize` methods pattern. `run` calls our two processor methods, `dot_product` and `filter`, directly. Each method accomplishes one of our two processing operations. `dot_product` isolates movement in the direction of gravity, and filter applies the low-pass and high-pass filters in sequence to remove jumpy and slow peaks. 

# Pedometer Functionality

Provided information about the person using the pedometer is available, we can measure more than just steps. Our pedometer will measure **distance traveled** and **time traveled**, as well as **steps taken**. 

## Distance Traveled
A mobile pedometer is generally used by one person. Distance travelled during a walk is calculated by multiplying the steps taken by the person's stride length. If the stride length is unknown, we can use optional user information like gender and height to approximate it. Ler's create a `User` class to encapsulate this related information. 

~~~~~~~
class User

  GENDER      = ['male', 'female']
  MULTIPLIERS = {'female' => 0.413, 'male' => 0.415}
  AVERAGES    = {'female' => 70,    'male' => 78}
  
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

At the top of our class, we define constants to avoid hardcoding "magic" numbers and strings throughout. Our initializer accepts `gender`, `height`, and `stride` all as optional arguments. Handling optional information is a common programming problem. If the optional parameters are passed in, our initializer sets instance variables of the same names, after some data formatting to allow for a case insensitive gender parameter to be passed in, as long as its defined in `GENDER`, and prevent a height and stride that is non-numerical or less than 0.

Even when all optional parameters are provided, the input stride takes precedence. If it's not provided, `calculate_stride` determines the most accurate stride length it can for the user. This is done with an `if` statement:

* The most accurate way to calculate stride beyond it being given directly is to use a person's height and a multiplier based on gender, provided we have a valid gender and height.
* A person's height is a better predictor of stride than their gender is. If we have a height but not a gender, we can multiply the height by the average of the two values in `MULTIPLIERS`. 
* If all we have is a gender, we can use the average stride length from `AVERAGES`.
* Finally, if we don't have anything, we can take the average of the two values in `AVERAGES` and use that as our stride. 

For the purposes of this discussion, let's assume that the values in `MULTIPLIERS` and `AVERAGES` have been determined from a large sample size of diverse people. Note that the further down the chain we get, the less accurate our stride length becomes. In any case, our `User` class determines the stride length as best as it can.

## Time Traveled

The time traveled is measured by dividing the number of data samples in our `Processor`'s `@parsed_data` by the sampling rate of the device. Since the rate has more to do with the trial walk itself than the user, and the `User` class in fact does not have to be aware of the sampling rate, this is a good time to create a very small `Trial` class.

~~~~~~~
class Trial

  attr_reader :name, :rate, :steps, :method

  def initialize(name = nil, rate = nil, steps = nil, method = nil)
    @name   = name
    @rate   = (rate.to_f.round > 0) ? rate.to_f.round : 100
    @steps  = steps.to_f.round if steps.to_s != '' && steps.to_f.round >= 0
    @method = method
  end

end
~~~~~~~

All of the attribute readers in `Trial` are set in the initializer based on optional parameters passed in:

* `name` is a name for the specific trial, to help differentiate between the different trials.
* `rate` is the sampling rate of the accelerometer during the trial.
* `steps` is used to set the actual steps taken, so that we can record the difference between the actual steps the user took and the ones our program counted.
* `method` is used to set the type of walk that is taken. Types of walks include walking with the device in a pocket, walking with the device in a bag, jogging with the device in a pocket, jogging with the device in a bag, etc.

Much like our `User` class, information is optional. We're given the opportunity to input details of the trial, if we have it, for more accurate end results. If we don't have those details, our program makes assumptions and is still able to produce results, albeit with a higher margin of error. Another similarity to our `User` class is the basic input data formatting in the initializer ensuring proper `@rate` and `@steps` values. 

## Steps Taken

It's time to implement our step counting strategy in code. So far, we have a `Processor` class that contains `@filtered_data`, which is our clean time series representing user acceleration in the direction of gravity. We also have classes that give us the necessary information about the user and the trial. What we're missing is a way to analyze `@filtered_data` with the information from `User` and `Trial`, and count steps, measure distance, and measure time. The analysis portion of our program is different from the data manipulation of the `Processor`, and different from the information collection and aggregation of the `User` and `Trial` classes. Let's create a new class called `Analyzer` to perform this data analysis.

~~~~~~~
require_relative 'processor'
require_relative 'user'
require_relative 'trial'

class Analyzer

  THRESHOLD = 0.09

  attr_reader :processor, :user, :trial, :steps, :distance, :time

  def self.run(processor, user = User.new, trial = Trial.new)
    analyzer = self.new(processor, user, trial)
    analyzer.measure_steps
    analyzer.measure_distance
    analyzer.measure_time
    analyzer
  end

  def initialize(processor, user = User.new, trial = Trial.new)
    raise 'Processor invalid.' unless processor.kind_of? Processor
    raise 'User invalid.'      unless user.kind_of? User
    raise 'Trial invalid.'     unless trial.kind_of? Trial

    @processor = processor
    @user      = user
    @trial     = trial
  end

  def measure_steps
    @steps = 0
    count_steps = true

    @processor.filtered_data.each_with_index do |data, i|
      if (data >= THRESHOLD) && (@processor.filtered_data[i-1] < THRESHOLD)
        next unless count_steps

        @steps += 1
        count_steps = false
      end

      count_steps = true if (data < 0) && (@processor.filtered_data[i-1] >= 0)
    end
  end

  def measure_distance
    @distance = @user.stride * @steps
  end

  def measure_time
    @time = @processor.filtered_data.count/@trial.rate
  end

end
~~~~~~~

The first thing we do in `Analyzer` is define a `THRESHOLD` constant. For the purposes of this discussion, let's assume we've analyzed numerous diverse data sets and determined a threshold value that accommodated the largest number of those data sets. The threshold can eventually become dynamic and vary with different users, based on the calculated versus actual steps they've taken. A learning algorithm, if you will.

Our `Analyzer`'s initializer take a mandatory `Processor` instance because we necessarily need a data set to work with, and optionally takes a `User` and `Trial` instance. Note that the default values for the `user` and `trial` parameters is a new instance of each. Remember how those classes both had default values and could handle zero input parameters? That functionality comes in handy here. The initializer raises exceptions if classes other than those expected are passed in, and sets the instance variables `@processor`, `@user`, and `@trial` to the passed in parameters. 

Aside from the initializer, the only other public method in `Analyzer` is `measure`, which calls `measure_steps`, `measure_distance`, and `measure_time`, in that order. All three methods are kept private so that an outside class can't call them out of order. Let's take a look at each.

### measure_steps

Finally! The step counting portion of our step counting app. The first thing we do in `measure_steps` is initialize two variables:

* `@steps` is used to count the number of steps.
* `count_steps` is used for hysteresis to determine if we're allowed to count steps at a point in time.

We then iterate through `@processor.filtered_data`. If the current value is greater than or equal to `THRESHOLD`, and the previous value was less than `THRESHOLD`, then we've crossed the threshold in the positive direction, which could indicate a step. The `unless` statement skips ahead to the next data point if `count_steps` is `false`, indicating that we've already counted a step for that peak. If we haven't, we increment `@steps` by 1, and set `count_steps` to `false` to prevent any more steps from being counted for that peak. The next `if` statement sets `count_steps` to true once our time series has crossed the x-axis in the negative direction, and we're on to the next peak. 

There we have it, the step counting portion of our program! Our `Processor` class did a lot of work to clean up the time series and remove frequencies that would result in counting false steps, so our actual step counting implementation is not overly complex. 

It's worth noting that we store the entire time series for the walk in memory. Our trials are all short walks, so that's not currently a problem, but we'd like to eventually be able to analyze long walks with large amounts of data. We'd ideally want to stream data in, only storing very small portions of the time series in memory. Keeping this future direction in mind, we've put in the work to ensure that we only need the current data point we're analyzing and the data point before it. Additionally, we've implemented hysteresis using a boolean value, so we don't need to look backward in the time series to ensure we've crossed the x-axis at 0, avoiding having to store more than two data points in memory at a time. 

There's a fine balance between accounting for likely future iterations of the product, and over engineering a solution for every conceivable product direction under the sun. In this case, it's reasonable to assume that we'll have to handle longer walks in the near future, and the costs of accounting for that in step counting are fairly low, so we've decided to include it in our implementation.

### measure_distance

The distance is measured by multiplying our user's stride by the number of steps. Since the distance depends on the step count, `measure_distance` is called after `measure_steps`. 

### measure_time

Time is calculated by dividing the total number of samples in `filtered_data` by the sampling rate. It follows, then, that time is calculated in numbers of seconds. 

# Adding Some Friendly

We're through the most labour intensive part of our program. Next, we'll build a web app to present the data in a format that is pleasing to a user. A web app naturally separates the data processing from the presentation of the data. Let's look at our app from a user's perspective before we dive into the code. 

## A User Scenario

When a user first enters the app by navigating to /uploads, they see a table of existing data, and a form to submit new data by uploading an accelerometer output file and trial and user information. 

![](chapter-figures/graffles/app1.png)\ 

Submitting the form stores the data to the file system, parses, processes, and analyzes it, and redirect back to /uploads with a new entry in the table. 

Clicking the **Detail** link for an entry presents the user with the following view:

![](chapter-figures/graffles/app3.png)\ 

The information presented includes values input by the user through the upload form, values calculated by our program, and graphs of the time series following the dot product operation, and again following filtering. The user can navigate back to uploads using the *Back to Uploads* link. 

Let's look at what the outlined functionality above implies for us, technically. We'll need two major components that we don't yet have:

1. A way to store and retrieve user input data.
2. A web application with a basic interface.

Let's examine each of these two requirements.

## 1. Storing and Retrieving Data

We need to store the text file containing the data samples, as well as the user and trial inputs associated with it. These user inputs are related to an upload, so we'll create an `Upload` class to keep track of, store, and load this data. 

~~~~~~~
require 'fileutils'
require_relative 'analyzer'

include FileUtils::Verbose

class Upload

  UPLOAD_DIRECTORY = 'public/uploads/'

  attr_reader :file_path, :parser, :processor, :user, :trial, :analyzer
  attr_reader :user_params, :trial_params

  def initialize(file_path = nil, input_data = nil, user_params = nil, trial_params = nil)
    if file_path
      @file_path = file_path
    elsif input_data
      @parser    = Parser.run(File.read(input_data))
      @processor = Processor.run(@parser.parsed_data)
      @user      = User.new(*user_params)
      @trial     = Trial.new(*trial_params)

      @file_path = UPLOAD_DIRECTORY + 
                   "#{user.gender}-#{user.height}-#{user.stride}_" +
                   "#{trial.name.to_s.gsub(/\s+/, '')}-" + 
                   "#{trial.rate}-" + 
                   "#{trial.steps}-" +
                   "#{trial.method}.txt"
    else 
      raise 'File name or input data must be passed in.'
    end
  end

  # -- Class Methods --------------------------------------------------------

  def self.create(input_data, user_params, trial_params)
    upload = self.new(nil, input_data, user_params, trial_params)
    cp(input_data, upload.file_path)
    upload
  end

  def self.find(file_path)
    self.new(file_path)
  end

  def self.all
    file_paths = Dir.glob(File.join(UPLOAD_DIRECTORY, "*"))
    file_paths.map { |file_path| self.new(file_path) }
  end

  # -- Instance Methods -----------------------------------------------------

  def parser
    @parser ||= Parser.run(File.read(file_path))
  end

  def processor
    @processor ||= Processor.run(parser.parsed_data)
  end

  def user
    @user ||= User.new(*file_name.first.split('-'))
  end

  def trial
    @trial ||= Trial.new(*file_name.last.split('-'))
  end

  def analyzer
    @analyzer ||= Analyzer.run(processor, user, trial)
  end

private

  def file_name
    @file_name ||= file_path.split('/').last.split('.txt').first.split('_')
  end

end
~~~~~~~

Our `Upload` class has three class-level methods used to store data to, and retrieve data from, the file system. 

### Storing Data

The `create` method stores a file that a user uploads using the browser upload field. The details of the user and trial information passed in through the browser input fields and dropdown boxes are stored in the filename. When using the browser upload field, the browser creates a temporary file that our app has access to. The first parameter passed in to `create`, `input_data`, is the location of the temporary file. The next two parameters, `user_params` and `trial_params`, are arrays of the values for a user and a trial, respectively.

~~~~~~~
> Upload.create('test/data/upload-1.txt', ['female', '168', '71'], ['1', 100', '10', 'run'])
~~~~~~~

The `create` method calls the initializer to create a new instance of `Upload`, passing in `nil` for the `file_path` parameter, and passing forward the remaining three parameters, `input_data`, `user_params`, and `trial_params`, unchanged. The initializer then creates and sets `Processor`, `User`, and `Trial` objects, and generates a filename using the attributes from these objects. Finally, the `create` method copies the temporary file to the file system under `public/uploads`, using the `file_path`. The `Upload` instance is returned. 

### Retrieving Data

We retrieve data from the file system using the `find` and `all` class methods.

Like the `create` method, `find` returns an instance of `Upload`. It calls into the initializer, passing in the `file_path`. All the initializer does if it's passed a `file_path` is set the `@file_path` instance variable. 

~~~~~~~
> Upload.find('public/uploads/female-168.0-70.0_1-100-100-walk-c.txt')
~~~~~~~

The `all` class method grabs all of the files in our `public/uploads` folder, and returns an array of `Upload` objects.

~~~~~~~
> Upload.all
~~~~~~~

### Working With an Instance of Upload

Our `Upload` class contains instance methods to access the `Processor`, `User`, `Trial`, and `Analyzer` objects directly. These objects are lazy loaded, to prevent creation until necessary. The `user` and `trial` instance methods use the `file_name` method to parse the file path to isolate the file name, and retrieve the necessary parameters from the file name to create `User` and `Trial` objects. The `processor` method reads the data from the file itself. Finally, the `analyzer` method uses `processor`, `user`, and `trial` to instantiate an `Analyzer` object and call `measure` on it, before returning the instance.

## Separation of Concerns in Upload

Once again, we've been wise to separate concerns in our program. All code related to storage and retrieval is contained in the `Upload` class. As our application grows, we'll likely want to use a database rather than saving everything to the file system. When the time comes for that, all we have to do it change the `Upload` class. This makes our refactoring simple and clean. 

In the future, we can save `User` and `Trial` objects to the database. The `create`, `find`, and `all` methods in `Upload` will then be relevant to `User` and `Trial` as well. That means we'd likely refactor those out into their own class to deal with data storage and retrieval in general, and each of our `User`, `Trial`, and `Upload` classes would inherit from that class. We might eventually add helper query methods to that class, and continue building it up from there. 

Let's move on to the web application side of our program to see how `Upload` will be helpful.

## 2. Building a Web Application

Web apps have been built many times over, so we'll leverage the important work of the open source community and use an existing framework to do the boring plumbing work for us. The Sinatra framework does just that. In the tool's own words, Sinatra is "a DSL for quickly creating web applications in Ruby". Perfect. Since we're building a web app, we'll need a web server. We'll use Thin as our web server, which is simple and certainly fast enough for our purposes. 

Sinatra and Thin can be installed as RubyGems, Ruby's popular library packaging system. We'll also use Bundler, a helpful tool to install and manage our RubyGems.

We'll start by creating a Gemfile:

~~~~~~~
source 'https://rubygems.org'

gem 'sinatra'
gem 'thin'
~~~~~~~

Once we run `bundle install` from our `pedometer` directory, we'll have Sinatra and the Thin web server.

Our web app will need to respond to HTTP requests, so we'll need a file that defines a route for each combination of HTTP method and URL. Let's call it `pedometer.rb`.

~~~~~~~
require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include ViewHelper

get '/uploads' do
  @uploads = Upload.all
  @error = "A #{params[:error]} error has occurred." if params[:error]

  erb :uploads
end

get '/upload/*' do |file_path|
  @upload = Upload.find(file_path)
  
  erb :upload
end

post '/create' do
  begin
    Upload.create(
      params[:processor][:file_upload][:tempfile], 
      params[:user].values,
      params[:trial].values
    )

    redirect '/uploads'
  rescue Exception => e
    redirect '/uploads?error=creation'
  end
end
~~~~~~~

Running `ruby pedometer.rb` from our app's directory starts the web server, allowing our app to respond to HTTP requests for each of our routes. Each route either retrieves data from, or stores data to, the file system through `Upload`, and then renders a view. Our views use erb, an emplementation of Embedded Ruby, which allows us to embed Ruby into HTML. The instance variables instantiated in our routes will be used directly in our views. The views simply display the data and aren't the focus of our app, so we we'll leave the code for them out of this chapter. 

### Helpers are Helpful

`pedometer.rb` loads in all models and helpers, and includes a `ViewHelper` module. As the name implies, `ViewHelper` contains methods that help the view. Let's look at one of the methods as an example. 

~~~~~~~
module ViewHelper
<...code truncated for brevity...>
  def format_time(time_sec)
    Time.at(time_sec.round).utc.strftime("%-H hr, %-M min, %-S sec")
  end
<...code truncated for brevity...>
end
~~~~~~~

`format_time` takes a time in seconds and formats it using Ruby's `strftime`, so that the view can display it as "`x` hr, `y` min, `z` sec", making it easier to comprehend than listing the total number of seconds. 

Here we once again see separation of concerns. To keep as much logic as possible out of the view, we leave the formatting to `ViewHelper`. Helpers group methods that have a similar purpose. It's sometimes tempting to jam all miscellaneous methods of a program into one helper, making it a dumping ground for code we don't know what to do with. If you find yourself doing this, always question where that method should belong, and which one of your classes or modules is responsible for the functionality of that method. It's wise to resist the temptation to put outcast code into helpers, as it can mask the problem of a poorly structured app.

## Back to Our App

Let's look at each of the routes in `pedometer.rb` individually. 

### get '/uploads'

With our server running, navigating to `http://localhost:4567/uploads` sends an HTTP GET request to our app, triggering our `get '/uploads'` code. The route retrieves all of the uploads in the file system and renders the `uploads` view, which displays a list of the uploads, and a form to submit new uploads. If an error parameter is included, the route will create an error string, and the `uploads` view will display the error.

### get '/upload/*'

Clicking the **Detail** link for each upload sends an HTTP GET to `/upload` with the file path for that upload. For example: `http://localhost:4567/upload/public/uploads/female-168.0-70.0_100-100-1-walk-c.txt`. The route code retirieves the upload through `Upload`, using the provided `file_path`, and renders the `upload` view. The upload view displays the details of the upload, including the charts, which are creating using a JavaScript library called HighCharts. 

### post '/create'

Our final route, an HTTP POST to `create`, is called when a user submits the form in the `uploads` view. The route creates a new `Upload`, using the `params` hash to grab the values input by the user through the form, and redirects back to `/uploads`. If an error occurs in the creation process, the redirect to `/uploads` includes an error parameter to let the user know that something went wrong. 

# A Fully Functional App

Voilà! We've built a fully functional app, with true applicability. 

The real world presents us with intricate, complex challenges. Software is uniquely capable of addressing these challenges at scale with minimal resources. As software engineers, we have the power to create positive change in our homes, our communities, and our world. Our training, academic or otherwise, likely equipped us with the problem-solving skills to write code that solves isolated, well-defined problems. As we grow and hone our craft, it's up to us to extend that training to address practical problems, tangled in with all of the messy realities of our world. I hope that this chapter gave you a taste of breaking down a real problem into small, addressable parts, and writing beautiful, clean, extensible code to build a solution. 

Here's to solving interesting problems in an endlessly exciting world.