require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

post '/create' do
  begin
    file = params[:parser][:file][:tempfile]
    user_params = params[:user].values
    device_params = params[:device].values
    build_with_params(File.read(file), user_params, device_params)

    @file_name = FileHelper.generate_file_name(@parser, @user, @device)
    cp(file, @file_name)

    set_match_filtered_data

    erb :trial
  rescue Exception => e
    redirect '/trials?error=creation'
  end
end

get '/trials' do
  @error = if params[:error]
    "A #{params[:error]} error has occurred. Please try again."
  end

  @data = []
  files = Dir.glob(File.join('public/uploads', "*"))
  files.each do |file|
    user_params, device_params = FileHelper.parse_file_name(file)
    build_with_params(File.read(file), user_params, device_params)

    @data << {:file => file, :analyzer => @analyzer}
  end

  erb :trials
end

get '/trial/*' do
  @file_name = params[:splat].first
  user_params, device_params = FileHelper.parse_file_name(@file_name)
  build_with_params(File.read(@file_name), user_params, device_params)

  set_match_filtered_data

  erb :trial
end

def build_with_params(data, user_params, device_params)
  @parser   = Parser.new(data)
  @user     = User.new(*user_params)
  @device   = Device.new(*device_params)
  @analyzer = Analyzer.new(@parser, @user, @device)
end

def set_match_filtered_data
  files = Dir.glob(File.join('public/uploads', "*"))
  files.delete(@file_name)

  match = if @parser.format == 'accelerometer'
    files.select { |f| @file_name == f.gsub('-g.', '-a.') }.first
  else
    files.select { |f| @file_name == f.gsub('-a.', '-g.') }.first
  end

  @match_filtered_data = if match
    parser = Parser.new(File.read(match))
    parser.filtered_data
  end
end
