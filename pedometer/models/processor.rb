class Processor

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

  attr_reader :data, :parsed_data, :dot_product_data, :filtered_data

  def initialize(data)
    @data = data

    parse
    dot_product
    filter
  end

  def parse
    # Extract numerical data into the format:
    # [ [ [x1t, y1t, z1t] ], ..., [ [xnt, ynt, znt] ] ]
    # OR
    # [ [ [x1u, y1u, z1u], [x1g, y1g, z1g] ], ..., 
    #   [ [xnu, ynu, znu], [xng, yng, zng] ] ]
    @parsed_data = @data.to_s.split(';').map { |i| i.split('|') }
                   .map { |i| i.map { |i| i.split(',').map(&:to_f) } }

    unless @parsed_data.map { |data| data.map(&:length).uniq }.uniq == [[3]]
      raise 'Bad Input. Ensure data is properly formatted.'
    end

    if @parsed_data.first.count == 1
      # Low-pass filter combined acceleration into the following format:
      # [ [ [x1u, x2u, ..., xnu], [x1g, x2g, ..., xng] ],
      #   [ [y1u, y2u, ..., ynu], [y1g, y2g, ..., yng] ],
      #   [ [z1u, z2u, ..., znu], [z1g, z2g, ..., zng] ] ]
      filtered_accl = @parsed_data.map(&:flatten).transpose.map do |total_accl|
        grav = chebyshev_filter(total_accl, GRAVITY)
        user = total_accl.zip(grav).map { |a, b| a - b }
        [user, grav]
      end

      # Format filtered acceleration into the following format:
      # [ [ [x1u, y1u, z1u], [x1g, y1g, z1g] ], ..., 
      #   [ [xnu, ynu, znu], [xng, yng, zng] ] ]
      @parsed_data = @parsed_data.length.times.map do |i|
        user = filtered_accl.map(&:first).map { |elem| elem[i] }
        grav = filtered_accl.map(&:last).map { |elem| elem[i] }
        [user, grav]
      end
    end
  end

  def dot_product
    @dot_product_data = @parsed_data.map do |data|
      data[0][0] * data[1][0] + 
      data[0][1] * data[1][1] + 
      data[0][2] * data[1][2]
    end
  end

  def filter
    low_pass_filtered_data = chebyshev_filter(@dot_product_data, SMOOTHING)
    @filtered_data = chebyshev_filter(low_pass_filtered_data, HIGHPASS)
  end

  def chebyshev_filter(input_data, coefficients)
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