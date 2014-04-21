class Parser

  attr_reader :data, :parsed_data, :dot_product_data

  def initialize(data)
    @data = data.to_s

    parse_raw_data
    dot_product_parsed_data
  end

private

  def parse_raw_data
    accl = @data.split(';').collect { |i| i.split('|') }    
    accl = accl.collect { |i| i.collect { |i| i.split(',').collect(&:to_f) } }
    split_accl = [accl.collect {|a| a.first}.transpose, 
                  accl.collect {|a| a.last}.transpose]

    user_accl, grav_accl   = split_accl
    user_x, user_y, user_z = user_accl
    grav_x, grav_y, grav_z = grav_accl
    
    @parsed_data = []
    accl.length.times do |i|
      @parsed_data << { x: user_x[i], y: user_y[i], z: user_z[i],
                        xg: grav_x[i], yg: grav_y[i], zg: grav_z[i] }
    end
  end

  def dot_product_parsed_data
    @dot_product_data = @parsed_data.collect do |data|
      data[:x] * data[:xg] + data[:y] * data[:yg] + data[:z] * data[:zg]
    end
  end

end