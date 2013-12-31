json.steps(@pedometer.steps)
json.distance(@pedometer.distance)
json.time("#{@pedometer.time} #{@pedometer.interval}")