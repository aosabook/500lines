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
      # TODO: Use @device.latency/(@device.latency + @device.rate)
      alpha = 0.97

      @device.data.split(';').each_with_index do |data, i|
        x, y, z = data.split(',').map { |coord| coord.to_f }

        if i == 0
          @parsed_data = [{:x => x, :y => y, :z => z, :xg => 0, :yg => 0, :zg => 0}]
        else

          # TODO: Change to Chebyshev filter
          # Chebyshev I, Apass = 1, Fpass = 0.3, Fs = 100
          # a0 = 1, a1 = -1.979133761292768, a2 = 0.979521463540373, 
          # b0 = 0.000086384997973502, b1 = 0.000172769995947004, b2 = 0.000086384997973502

          xg = (alpha * @parsed_data[i-1][:xg] + (1-alpha) * x).round(3)
          yg = (alpha * @parsed_data[i-1][:yg] + (1-alpha) * y).round(3)
          zg = (alpha * @parsed_data[i-1][:zg] + (1-alpha) * z).round(3)

          @parsed_data << {:x => x-xg, :y => y-yg, :z => z-zg,
                           :xg => xg, :yg => yg, :zg => zg}
        end
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