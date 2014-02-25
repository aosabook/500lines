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
# - Clean up file name creation (don't allow spaces in trial name)
# - Add drop-downs for method (select a few), step count (integer only), gender (pre-populate)
# - Change device meta data to unqiue params?
post '/create' do
  begin
    temp_file_path = params[:device][:file][:tempfile].path
    @file_name = params[:device][:file][:filename]
    
    @device = Device.new(:data => File.read(temp_file_path), :meta_data => params[:device][:meta_data].values.join(','), :rate => params[:device][:rate])
    @parser = Parser.new(@device)
    user = User.new(params[:user])
    @analyzer = Analyzer.new(@parser, user)
    @analyzer.measure

    cp(temp_file_path, "public/uploads/#{user.gender}-#{user.height}-#{user.stride}_#{@device.rate}-#{@device.method}-#{@device.steps}-#{@device.trial}-#{@device.format[0]}")

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

# TODO: 
# - Add file parser module?
# - Refactor out some common functionality
get '/data' do
  begin
    @data = []
    files = Dir.glob(File.join('public/uploads', "*"))
    files.each do |file|
      file_name = file.split('/').last
      user_data = file_name.split('_').first.split('-')
      user = User.new(gender: user_data[0], height: user_data[1], stride: user_data[2])

      device_data = file_name.split('_').last.gsub('.txt','').split('-')
      rate = device_data.delete_at(0)
      meta_data = device_data.join(',')
      device = Device.new(:data => File.read(file), :meta_data => meta_data, :rate => rate)

      parser = Parser.new(device)
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
    user_data = @file_name.split('/').last.split('_').first.split('-')
    user = User.new(gender: user_data[0], height: user_data[1], stride: user_data[2])

    device_data = @file_name.split('/').last.split('_').last.gsub('.txt','').split('-')
    rate = device_data.delete_at(0)
    meta_data = device_data.join(',')
    @device = Device.new(:data => File.read(@file_name), :meta_data => meta_data, :rate => rate)

    @parser = Parser.new(@device)
    @analyzer = Analyzer.new(@parser, user)
    @analyzer.measure

    files = Dir.glob(File.join('public/uploads', "*"))
    match = if @file_name.include?('-a.txt')
      files.select {|f| f == @file_name.gsub('-a.txt', '-g.txt')}.first
    elsif @file_name.include?('-g.txt')
      files.select {|f| f == @file_name.gsub('-g.txt', '-a.txt')}.first
    end

    @file_name = @file_name.split('/').last

    if match
      device = Device.new(:data => File.read(match))
      parser = Parser.new(device)
      @match_filtered_data = parser.filtered_data
    end

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

