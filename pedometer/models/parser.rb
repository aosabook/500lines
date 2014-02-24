require './models/device.rb'

class Parser

  attr_reader :device, :parsed_data, :dot_product_data, :filtered_data

  def initialize(device)
    unless (@device = device).kind_of? Device
      raise "A Device object must be passed in."
    end
    
    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

private

  def parse_raw_data
    case @device.format
    when 'accelerometer'
      a0 = 1
      a1 = -1.979133761292768
      a2 = 0.979521463540373
      b0 = 0.000086384997973502
      b1 = 0.000172769995947004
      b2 = 0.000086384997973502

      coordinates = @device.data.split(';')
      x_series = coordinates.inject([]) {|a, data| a << data.split(',')[0].to_f }
      y_series = coordinates.inject([]) {|a, data| a << data.split(',')[1].to_f }
      z_series = coordinates.inject([]) {|a, data| a << data.split(',')[2].to_f }
      
      xg_series = chebyshev_filter(x_series, a0, a1, a2, b0, b1, b2)
      yg_series = chebyshev_filter(y_series, a0, a1, a2, b0, b1, b2)
      zg_series = chebyshev_filter(z_series, a0, a1, a2, b0, b1, b2)

      @parsed_data = []
      coordinates.length.times do |i|
        @parsed_data << {:x => (x_series[i]-xg_series[i]).round(4), 
                         :y => (y_series[i]-yg_series[i]).round(4), 
                         :z => (z_series[i]-zg_series[i]).round(4), 
                         :xg => xg_series[i].round(4), :yg => yg_series[i].round(4), :zg => zg_series[i].round(4)}
      end
    when 'gravity'
      @parsed_data = @device.data.split(';').inject([]) do |a, data|
        accl, grav = data.split('|')
        accl = accl.split(',')
        grav = grav.split(',')

        a << {:x => accl[0].to_f, :y => accl[1].to_f, :z => accl[2].to_f,
              :xg => grav[0].to_f, :yg => grav[1].to_f, :zg => grav[2].to_f}
        a
      end
    end    
  end

  def dot_product_parsed_data
    @dot_product_data = @parsed_data.inject([]) do |a, data|
      a << (data[:x]*data[:xg] + data[:y]*data[:yg] + data[:z]*data[:zg]).round(4)
      a
    end
  end

  # Chebyshev II, Astop = 2, Fstop = 5, Fs = 100
  def filter_dot_product_data
    @filtered_data = chebyshev_filter(@dot_product_data, 
      1, -1.80898117793047, 0.827224480562408, 0.095465967120306, -0.172688631608676, 0.095465967120306)
  end

  def chebyshev_filter(input_data, a0, a1, a2, b0, b1, b2)
    output_data = [0,0]
    input_data.length.times do |i|
      next if i < 2
      output_data << (input_data[i]*b0 + 
                      input_data[i-1]*b1 + 
                      input_data[i-2]*b2 -
                      output_data[i-1]*a1 -
                      output_data[i-2]*a2).round(4)
    end
    output_data
  end
end