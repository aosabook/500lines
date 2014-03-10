require 'sinatra'
require 'sinatra/jbuilder'
Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

# TODO: 
# - Capture exceptions and redirect to /data
# - Bad rescues
# - Make device_params shorter
post '/create' do
  begin
    file = params[:device][:file][:tempfile]
    user_params = params[:user].values
    device_params = [File.read(file), params[:device][:rate], params[:device][:method], params[:device][:steps], params[:device][:trial]]
    build_with_params(user_params, device_params)

    @file_name = FileHelper.generate_file_name(@user, @device)
    cp(file, "public/uploads/" + @file_name + '.txt')

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

# TODO: rename actions
get '/data' do
  begin
    @data = []
    files = Dir.glob(File.join('public/uploads', "*"))
    files.each do |file|
      user_params, device_params = FileHelper.parse_file_name(file)
      device_params = device_params.unshift(File.read(file))
      build_with_params(user_params, device_params)

      @data << {:file => file, :device => @device, :steps => @analyzer.steps, :user => @user}
    end

    erb :data
  rescue Exception => e
    [400, e.message]
  end
end

get '/detail/*' do
  begin
    @file_name = params[:splat].first
    user_params, device_params = FileHelper.parse_file_name(@file_name)
    device_params = device_params.unshift(File.read(@file_name))
    build_with_params(user_params, device_params)

    files = Dir.glob(File.join('public/uploads', "*"))
    files.delete(@file_name)
    
    match = if @device.format == 'accelerometer'
      files.select { |f| @file_name == f.gsub('-g.', '-a.') }.first
    else
      files.select { |f| @file_name == f.gsub('-a.', '-g.') }.first
    end

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

# TODO: Should initialization process data or should that be called explicitly? 
def build_with_params(user_params, device_params)
  @user     = User.new(*user_params)
  @device   = Device.new(*device_params)
  @parser   = Parser.new(@device)
  @analyzer = Analyzer.new(@parser, @user)
  @analyzer.measure
end
