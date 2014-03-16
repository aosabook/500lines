class ViewHelper

  DISTANCE = { cm_per_m: 100, cm_per_km: 100000, m_per_km: 1000 }
  TIME = { sec_per_min: 60.0, sec_per_hr: 3600.0, min_per_hr: 60}

  # TODO: Can this be cleaner? Does it exist in Ruby already?
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

  # TODO: Does this exist in Ruby? Should be 4 hours, 23 mins, 2 secs
  def self.format_time(time_sec)
    if time_sec >= TIME[:sec_per_hr]
      "#{(time_sec/TIME[:sec_per_hr]).round(1)} hr"
    elsif time_sec >= TIME[:sec_per_min]
      time = (time_sec/TIME[:sec_per_min]).round(1)
      (time == TIME[:min_per_hr]) ? "1.0 hr" : "#{time} min"
    else
      time = time_sec.round
      (time == TIME[:sec_per_min]) ? "1.0 min" : "#{time} sec"
    end
  end

end