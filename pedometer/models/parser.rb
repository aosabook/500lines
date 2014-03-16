require_relative '../models/device'

class Parser

  GRAVITY_COEFFICIENTS = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  
  # Chebyshev II, Astop = 2, Fstop = 5, Fs = 100
  SMOOTHING_COEFFICIENTS = {
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }  

  attr_reader :device, :parsed_data, :dot_product_data, :filtered_data

  # TODO: 
  # - Assignment should be first
  # - 78 character limit to prevent long lines
  # - Pass in data as well?
  def initialize(device)
    raise "A Device object must be passed in." unless (@device = device).kind_of? Device
    
    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

private

  # TODO:
  # - Combine, try to split on pipe, determine format from that
  def parse_raw_data
    case @device.format
    when 'accelerometer'
      coordinates = @device.data.split(';')
      # TODO:
      # - x_series = coordinates.collect {|data| data.split(',')[0].to_f } to replace inject?
      # - [:x, :y, :z].each_with_index do |axis, index| to remove repetition
      x_series = coordinates.collect {|data| data.split(',')[0].to_f }
      y_series = coordinates.collect {|data| data.split(',')[1].to_f }
      z_series = coordinates.collect {|data| data.split(',')[2].to_f }
      
      xg_series = chebyshev_filter(x_series, GRAVITY_COEFFICIENTS)
      yg_series = chebyshev_filter(y_series, GRAVITY_COEFFICIENTS)
      zg_series = chebyshev_filter(z_series, GRAVITY_COEFFICIENTS)

      @parsed_data = []
      coordinates.length.times do |i|
        @parsed_data << {:x => (x_series[i]-xg_series[i]).round(4), 
                         :y => (y_series[i]-yg_series[i]).round(4), 
                         :z => (z_series[i]-zg_series[i]).round(4), 
                         :xg => xg_series[i].round(4), 
                         :yg => yg_series[i].round(4), 
                         :zg => zg_series[i].round(4)}
      end
    when 'gravity'
      @parsed_data = @device.data.split(';').collect do |data|
        accl, grav = data.split('|')
        accl = accl.split(',')
        grav = grav.split(',')

        {:x => accl[0].to_f, :y => accl[1].to_f, :z => accl[2].to_f,
         :xg => grav[0].to_f, :yg => grav[1].to_f, :zg => grav[2].to_f}
      end
    end    
  end

  def dot_product_parsed_data
    # TODO: 
    # - Do you need to round?
    # - Spaces on *
    @dot_product_data = @parsed_data.collect do |data|
      (data[:x] * data[:xg] + data[:y] * data[:yg] + data[:z] * data[:zg]).round(4)
    end
  end

  def filter_dot_product_data
    @filtered_data = chebyshev_filter(@dot_product_data, SMOOTHING_COEFFICIENTS)
  end

  def chebyshev_filter(input_data, coefficients)
    # TODO: 
    # - Fix loop to start from index 2
    # - Do we need to round?
    output_data = [0,0]
    input_data.length.times do |i|
      next if i < 2
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