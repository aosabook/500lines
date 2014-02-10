require 'sinatra'
require 'sinatra/jbuilder'
require './models/analyzer.rb'
require './models/device.rb'

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
    @data = {}
    files = Dir.glob(File.join('test/data/female', "*")) + Dir.glob(File.join('test/data/male', "*"))

    files.each do |file|
      next if FileTest.directory?(file) || file.include?('walking-1-g-false-step.txt')

      device = Device.new(:data => File.read(file), :rate => 100)
      parser = Parser.new(device)

      analyzer = Analyzer.new(parser)
      analyzer.measure_steps
      @data[file] = analyzer.steps
    end
    
    @pairs = []
    @data.keys.each do |file|
      match = if file.include?('-a-')
        @data.keys.select {|f| f == file.gsub('-a-', '-g-')}.first
      elsif file.include?('-g-')
        @data.keys.select {|f| f == file.gsub('-g-', '-a-')}.first
      end
      
      if match
        @pairs << [file, match].sort
      end
    end
    @pairs.uniq!

    erb :data
  rescue Exception => e
    [400, e.message]
  end
end

get '/data/compare/*/and/*' do
  begin
    @data = []

    [params[:splat].first, params[:splat].last].each do |file|
      device = Device.new(:data => File.read(file), :rate => 100)
      parser = Parser.new(device)

      analyzer = Analyzer.new(parser)
      analyzer.measure_steps
      @data << {:file => file, :steps => analyzer.steps, :filtered_data => parser.filtered_data}
    end

    erb :compare
  rescue Exception => e
    [400, e.message]
  end
end
