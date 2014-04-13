class ViewHelper

  DISTANCE = { cm_per_m: 100, cm_per_km: 100000, m_per_km: 1000 }

  # TODO: Can this be cleaner?
  # - 2 is a magic number here. Maybe we can add it as a class variable?
  # - (expr).round(2) is repeated multiple times within this function. It might be a good idea to define an anonymous function to reduce code repetition.
  def self.format_distance(distance_cm)
    distance_cm = distance_cm.round(2)
    if distance_cm >= DISTANCE[:cm_per_km]
      "#{(distance_cm/DISTANCE[:cm_per_km]).round(2)} km"
    elsif distance_cm >= DISTANCE[:cm_per_m]
      distance = (distance_cm/DISTANCE[:cm_per_m]).round(2)
      (distance == DISTANCE[:m_per_km]) ? "1.0 km" : "#{distance} m"
    else
      (distance_cm == DISTANCE[:cm_per_m]) ? "1.0 m" : "#{distance_cm} cm"
    end
  end

  def self.format_time(time_sec)
    Time.at(time_sec.round).utc.strftime("%-H hr, %-M min, %-S sec")
  end

  def self.limit_1000(series)
    series.to_a[0..999]
  end

end