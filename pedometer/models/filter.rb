class Filter

  GRAVITY = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  
  # Direct form I, Chebyshev II, type = low-pass, 
  # Astop = 2, Fstop = 5, Fs = 100, Direct Form I
  SMOOTHING = {
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }

  # Direct form I, Chebyshev II, type = high-pass, 
  # Fs = 100, Fstop = 0.5, Astop = 20, order = 2, 
  HIGHPASS = {
    alpha: [1, -1.905384612118461, 0.910092542787947], 
    beta:  [0.953986986993339, -1.907503180919730, 0.953986986993339]
  }

  def self.chebyshev_filter(input_data, coefficients)
    output_data = [0,0]
    (2..input_data.length-1).each do |i|
      output_data << coefficients[:alpha][0] * 
                     (input_data[i]   * coefficients[:beta][0] +
                     input_data[i-1]  * coefficients[:beta][1] +
                     input_data[i-2]  * coefficients[:beta][2] -
                     output_data[i-1] * coefficients[:alpha][1] -
                     output_data[i-2] * coefficients[:alpha][2])
    end
    output_data
  end

end