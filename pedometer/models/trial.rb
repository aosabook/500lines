class Trial

  attr_reader :name, :rate, :steps, :method

  def initialize(name = nil, method = nil, rate = nil, steps = nil)
    @name   = name.to_s.delete(' ')
    @method = method.to_s.delete(' ')
    @rate   = Integer(rate.to_s) unless rate.to_s.empty? rescue raise('Invalid rate')
    @steps  = Integer(steps.to_s) unless steps.to_s.empty? rescue raise('Invalid steps')

    raise('Invalid rate') if @rate && (@rate <= 0)
    raise('Invalid steps') if @steps && (@steps < 0)

    @rate ||= 100    
  end

end
