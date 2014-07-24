class Processor

  GRAVITY_COEFF = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  
  # Chebyshev II, Astop = 2, Fstop = 5, Fs = 100
  SMOOTHING_COEFF = {
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }  

  FORMAT_COMBINED  = 'combined'
  FORMAT_SEPARATED = 'separated'

  attr_reader :data, :format, :parsed_data, :dot_product_data, :filtered_data

  # TODO: 
  # Should the methods be moved out of the initializer? Or, renamed to:
  # set_parsed_data, set_dot_product_data, set_filtered_data?
  def initialize(data)
    @data = data.to_s

    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

  # TODO:
  # You should be more explicit with your exception catching. It's better to 
  # have specific exceptions that you except to be raised, and have logic to handle those cases.
  def parse_raw_data
    # Extract numerical data into the format:
    # [ [ [x1t, y1t, z1t] ], ..., [ [xnt, ynt, znt] ] ]
    # OR
    # [ [ [x1u, y1u, z1u], [x1g, y1g, z1g] ], ..., 
    #   [ [xnu, ynu, znu], [xng, yng, zng] ] ]
    accl = @data.split(';').map { |i| i.split('|') }
                .map { |i| i.map { |i| i.split(',').map(&:to_f) } }
    
    @format = if accl.first.count == 1
      # TODO: Try to combine the two loops below. Easier to explain. 
      #       See branch parser-option-2.

      # Low-pass filter combined acceleration into the following format:
      # [ [ [x1u, x2u, ..., xnu], [x1g, x2g, ..., xng] ],
      #   [ [y1u, y2u, ..., ynu], [y1g, y2g, ..., yng] ],
      #   [ [z1u, z2u, ..., znu], [z1g, z2g, ..., zng] ], ]
      filtered_accl = accl.map(&:flatten).transpose.map do |total_accl|
        grav = chebyshev_filter(total_accl, GRAVITY_COEFF)
        user = total_accl.zip(grav).map { |a, b| a - b }
        [user, grav]
      end

      # Format filtered acceleration into the following format:
      # [ [ [x1u, y1u, z1u], [x1g, y1g, z1g] ], ..., 
      #   [ [xnu, ynu, znu], [xng, yng, zng] ] ]
      accl = accl.length.times.map do |i| 
        coordinate_user = filtered_accl.map(&:first).map { |elem| elem[i] }
        coordinate_grav = filtered_accl.map(&:last).map { |elem| elem[i] }

        [coordinate_user, coordinate_grav]
      end

      FORMAT_COMBINED
    else
      FORMAT_SEPARATED
    end

    # Set @parsed_data to standard format
    @parsed_data = []
    accl.each do |point|
      unless point.map(&:length) == [3, 3]
        raise 'Bad Input. Ensure data is properly formatted.'
      end
      
      @parsed_data << { x: point[0][0], y: point[0][1], z: point[0][2],
                        xg: point[1][0], yg: point[1][1], zg: point[1][2] }
    end
  rescue
    raise 'Bad Input. Ensure data is properly formatted.'
  end

  def dot_product_parsed_data
    @dot_product_data = @parsed_data.map do |data|
      data[:x] * data[:xg] + data[:y] * data[:yg] + data[:z] * data[:zg]
    end
  end

  def filter_dot_product_data
    @filtered_data = chebyshev_filter(@dot_product_data, SMOOTHING_COEFF)
  end

  def chebyshev_filter(input_data, coefficients)
    output_data = [0,0]
    (2..input_data.length-1).each do |i|
      output_data << coefficients[:alpha][0] * 
                      (input_data[i]    * coefficients[:beta][0] +
                       input_data[i-1]  * coefficients[:beta][1] +
                       input_data[i-2]  * coefficients[:beta][2] -
                       output_data[i-1] * coefficients[:alpha][1] -
                       output_data[i-2] * coefficients[:alpha][2])
    end
    output_data
  end

end