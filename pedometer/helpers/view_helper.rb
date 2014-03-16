class ViewHelper

  # TODO: Can this be cleaner?
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

  def self.format_time(time_sec)

  end

end