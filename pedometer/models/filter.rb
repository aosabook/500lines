class Filter

  COEFFICIENTS = {
    low_0_hz: {
      alpha: [1, -1.979133761292768, 0.979521463540373],
      beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
    },
    low_5_hz: { # Direct form I, Chebyshev II, type = low-pass, Astop = 2, Fstop = 5, Fs = 100, Direct Form I
      alpha: [1, -1.80898117793047, 0.827224480562408], 
      beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
    },
    high_1_hz: { # Direct form I, Chebyshev II, type = high-pass, Fs = 100, Fstop = 0.5, Astop = 20, order = 2, 
      alpha: [1, -1.905384612118461, 0.910092542787947], 
      beta:  [0.953986986993339, -1.907503180919730, 0.953986986993339]
    }
  }

  def self.run(data, type)
    filtered_data = [0,0]
    (2..data.length-1).each do |i|
      filtered_data << COEFFICIENTS[type][:alpha][0] * 
                      (data[i]            * COEFFICIENTS[type][:beta][0] +
                       data[i-1]          * COEFFICIENTS[type][:beta][1] +
                       data[i-2]          * COEFFICIENTS[type][:beta][2] -
                       filtered_data[i-1] * COEFFICIENTS[type][:alpha][1] -
                       filtered_data[i-2] * COEFFICIENTS[type][:alpha][2])
    end
    filtered_data
  end

end