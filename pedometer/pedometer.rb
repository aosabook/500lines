# TODO
# - This module seems to be the web interface for your pedometer service. 
# However it has some file handling logic that should probably be in another module. 
# This allows you to separate responsibilities. 
# And if in the future you decided to change how data is stored (maybe from file objects to a small database) 
# refactoring your code will be a lot easier.

require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

# TODO
# - file as a variable name is a little too generic. Maybe file_name, or file_object.
# - Is file sanitized here? We don't want to be passing around untrusted data, especially not if it's touching the filesystem.
post '/create' do
  begin
    file_upload   = params[:parser][:file_upload][:tempfile]
    user_params   = params[:user].values
    device_params = params[:device].values
    build_with_params(File.read(file_upload), user_params, device_params)

    @file_name = FileHelper.generate_file_name(@parser, @user, @device)
    cp(file_upload, @file_name)

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
  @analyzer.measure
end

# TODO
# - Can you add a comment here to explain what's going on? We spent a few minutes looking at it and couldn't figure it out.
def set_match_filtered_data
  files = Dir.glob(File.join('public/uploads', "*"))
  files.delete(@file_name)

  match = if @parser.is_data_accelerometer?
    files.select { |f| @file_name == f.gsub('-g.', '-a.') }.first
  else
    files.select { |f| @file_name == f.gsub('-a.', '-g.') }.first
  end

  @match_filtered_data = if match
    parser = Parser.new(File.read(match))
    parser.filtered_data
  end
end
