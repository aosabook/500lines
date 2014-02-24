require 'sinatra'
require 'sinatra/jbuilder'
require './models/analyzer.rb'
require './models/device.rb'
require './models/user.rb'

get '/metrics' do
  begin
    device = Device.new(params[:device])
    parser = Parser.new(device)
    user   = User.new(params[:user])

    @analyzer = Analyzer.new(parser, user)
    @analyzer.measure    

    [200, jbuilder(:index)]
  rescue Exception => e
    [400, e.message]
  end
end

get '/data' do
  begin
    @data = []
    files = Dir.glob(File.join('test/data/female', "*")) + Dir.glob(File.join('test/data/male', "*"))

    files.each do |file|
      next if FileTest.directory?(file) || file.include?('walking-1-g-false-step.txt')

      meta_data = /\w+-\d+-[a,g]-\d/.match(file)[0].gsub(/-[a,g]-|-/,',')
      device = Device.new(:data => File.read(file), :meta_data => meta_data, :rate => 100)
      parser = Parser.new(device)
      user = User.new(:gender => file.split('/')[2])
      analyzer = Analyzer.new(parser, user)
      analyzer.measure_steps
      @data << {:file => file, :device => device, :steps => analyzer.steps, :user => user}
    end

    erb :data
  rescue Exception => e
    [400, e.message]
  end
end

get '/detail/*' do
  begin
    @file = params[:splat].first
    @device = Device.new(:data => File.read(@file), :rate => 100)
    @parser = Parser.new(@device)

    @analyzer = Analyzer.new(@parser)
    @analyzer.measure

    files = Dir.glob(File.join('test/data/female', "*")) + Dir.glob(File.join('test/data/male', "*"))
    match = if @file.include?('-a-')
      files.select {|f| f == @file.gsub('-a-', '-g-')}.first
    elsif @file.include?('-g-')
      files.select {|f| f == @file.gsub('-g-', '-a-')}.first
    end

    if match
      device = Device.new(:data => File.read(match), :rate => 100)
      parser = Parser.new(device)
      @match_filtered_data = parser.filtered_data
    end

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

