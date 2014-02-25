require 'sinatra'
require 'sinatra/jbuilder'
require './models/analyzer.rb'
require './models/device.rb'
require './models/user.rb'

include FileUtils::Verbose

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

# TODO: 
# - Capture exceptions and redirect to /data
# - Recreate file name from params and store
post '/create' do
  begin
    temp_file_path = params[:device][:file][:tempfile].path
    @file_name = params[:device][:file][:filename]
    @device = Device.new(:data => File.read(temp_file_path), :rate => params[:device][:rate])
    @parser = Parser.new(@device)

    @analyzer = Analyzer.new(@parser)
    @analyzer.measure

    cp(temp_file_path, "public/uploads/#{@file_name}")

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

get '/data' do
  begin
    @data = []
    files = Dir.glob(File.join('test/data/female', "*")) + Dir.glob(File.join('test/data/male', "*")) + Dir.glob(File.join('public/uploads', "*"))

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
    @file_name = params[:splat].first
    meta_data = /\w+-\d+-[a,g]-\d/.match(@file_name)[0].gsub(/-[a,g]-|-/,',')
    @device = Device.new(:data => File.read(@file_name), :rate => 100)
    @parser = Parser.new(@device)

    @analyzer = Analyzer.new(@parser)
    @analyzer.measure

    files = Dir.glob(File.join('test/data/female', "*")) + Dir.glob(File.join('test/data/male', "*"))
    match = if @file_name.include?('-a-')
      files.select {|f| f == @file_name.gsub('-a-', '-g-')}.first
    elsif @file_name.include?('-g-')
      files.select {|f| f == @file_name.gsub('-g-', '-a-')}.first
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

