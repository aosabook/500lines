class ViewHelper

  # TODO: Can this be cleaner? Does it exist in Ruby already?
  def self.format_distance(distance_cm)
    distance_cm = distance_cm.round(2)
    if distance_cm >= 100000
      "#{(distance_cm/100000).round(2)} km"
    elsif distance_cm >= 100
      distance = (distance_cm/100).round(2)
      (distance == 1000) ? "1.0 km" : "#{distance} m"
    else
      (distance_cm == 100) ? "1.0 m" : "#{distance_cm} cm"
    end
  end

  # TODO: Does this exist in Ruby? Should be 4 hours, 23 mins, 2 secs
  def self.format_time(time_sec)
    if time_sec >= 3600
      "#{(time_sec/3600.0).round(1)} hr"
    elsif time_sec >= 60
      time = (time_sec/60.0).round(1)
      (time == 60) ? "1.0 hr" : "#{time} min"
    else
      time = time_sec.round
      (time == 60) ? "1.0 min" : "#{time} sec"
    end
  end

end