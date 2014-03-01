require 'sinatra'
require 'sinatra/jbuilder'
require './models/analyzer.rb'
require './models/device.rb'
require './models/user.rb'
require './helpers/file_helper.rb'
require './helpers/hash_helper.rb'

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
post '/create' do
  begin
    temp_file_path = params[:device][:file][:tempfile]
    
    @device = Device.new({data: File.read(temp_file_path)}.merge(params[:device].symbolize_keys))
    @parser = Parser.new(@device)
    user = User.new(params[:user].symbolize_keys)
    @analyzer = Analyzer.new(@parser, user)
    @analyzer.measure

    @file_name = FileHelper.generate_file_name(user, @device)
    
    cp(temp_file_path, "public/uploads/" + @file_name)

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

# TODO: 
# - Refactor out some common functionality
get '/data' do
  begin
    @data = []
    files = Dir.glob(File.join('public/uploads', "*"))
    files.each do |file|
      file_name = file.split('/').last
      user_params, device_params = FileHelper.parse_file_name(file_name).values

      user = User.new(user_params)
      device = Device.new({:data => File.read(file)}.merge(device_params))
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

