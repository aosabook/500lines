class Parser

  attr_reader :data, :parsed_data

  def initialize(data)
    @data = data.to_s

    parse_raw_data
  end

private

  def parse_raw_data
    # Split on ; first, followed by |, to get data into the format:
    # [["x1,y1,z1", "xg1,yg1,zg1"], ..., ["xn,yn,zn", "xgn,ygn,zgn"]]
    accl = @data.split(';').collect { |i| i.split('|') }
    
    # Convert data to floats:
    # [[x1,y1,z1], [xg1,yg1,zg1]], ..., [[xn,yn,zn], [xgn,ygn,zgn]]]
    accl = accl.collect { |i| i.collect { |i| i.split(',').collect(&:to_f) } }

    # Split acceleration data into the following format:
    # [ [ [x1, x2, ..., xn],    [y1, y2, ..., yn],    [z1, z2, ..., zn] ],
    #   [ [xg1, xg2, ..., xgn], [yg1, yg2, ..., ygn], [zg1, zg2, ..., zgn] ] ]
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

end