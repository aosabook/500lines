require_relative 'filter'
require_relative 'parser'

class Processor

  attr_reader :dot_product_data, :filtered_data

  def initialize(data)
    @dot_product_data = dot_product(data)
    @filtered_data    = filter(@dot_product_data)
  end

  def dot_product(data)
    data.map do |data|
      data[0][0] * data[1][0] + 
      data[0][1] * data[1][1] + 
      data[0][2] * data[1][2]
    end
  end

  def filter(data)
    low_pass_filtered_data = Filter.chebyshev_filter(data, Filter::SMOOTHING)
    Filter.chebyshev_filter(low_pass_filtered_data, Filter::HIGHPASS)
  end

end