require 'sinatra'
require 'sinatra/jbuilder'
Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

# TODO: 
# - Capture exceptions and redirect to /trials
# - Bad rescues
# - Make device_params shorter
post '/create' do
  begin
    file = params[:parser][:file][:tempfile]
    user_params = params[:user].values
    device_params = [params[:device][:rate], params[:device][:method], params[:device][:steps], params[:device][:trial]]
    build_with_params(File.read(file), user_params, device_params)

    @file_name = FileHelper.generate_file_name(@parser, @user, @device)
    cp(file, "public/uploads/" + @file_name + '.txt')

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

get '/trials' do
  begin
    @data = []
    files = Dir.glob(File.join('public/uploads', "*"))
    files.each do |file|
      user_params, device_params = FileHelper.parse_file_name(file)
      build_with_params(File.read(file), user_params, device_params)

      @data << {:file => file, :analyzer => @analyzer}
    end

    erb :data
  rescue Exception => e
    [400, e.message]
  end
end

get '/trial/*' do
  begin
    @file_name = params[:splat].first
    user_params, device_params = FileHelper.parse_file_name(@file_name)
    build_with_params(File.read(@file_name), user_params, device_params)

    files = Dir.glob(File.join('public/uploads', "*"))
    files.delete(@file_name)
    
    match = if @parser.format == 'accelerometer'
      files.select { |f| @file_name == f.gsub('-g.', '-a.') }.first
    else
      files.select { |f| @file_name == f.gsub('-a.', '-g.') }.first
    end

    if match
      parser = Parser.new(File.read(match))
      @match_filtered_data = parser.filtered_data
    end

    erb :detail
  rescue Exception => e
    [400, e.message]
  end
end

# TODO: Should initialization process data or should that be called explicitly? 
def build_with_params(data, user_params, device_params)
  @parser   = Parser.new(data)
  @user     = User.new(*user_params)
  @device   = Device.new(*device_params)
  @analyzer = Analyzer.new(@parser, @user, @device)
end
