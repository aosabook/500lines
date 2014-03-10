class ViewHelper

  def self.format_distance(distance_cm)
    distance_cm = distance_cm.round(2)
    if distance_cm >= 100000
      "#{(distance_cm/100000).round(2)} km"
    elsif distance_cm >= 100
      "#{(distance_cm/100).round(2)} m"
    else
      "#{(distance_cm).round(2)} cm"
    end
  end

  def self.format_time(time_sec)

  end

end