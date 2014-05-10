# TODO
# - This module seems to be the web interface for your pedometer service. 
# However it has some file handling logic that should probably be in another module. 
# This allows you to separate responsibilities. 
# And if in the future you decided to change how data is stored (maybe from file objects to a small database) 
# refactoring your code will be a lot easier.

require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

get '/trials' do
  @error = if params[:error]
    "A #{params[:error]} error has occurred. Please try again."
  end

  @data = Trial.all.inject([]) do |a, trial|
    a << {:file_name => trial.file_name, :analyzer => trial.analyzer}
    a
  end
  
  erb :trials
end

get '/trial/*' do
  @trial = Trial.find(params[:splat].first)

  set_match_filtered_data
  
  erb :trial
end

# TODO
# - Is file sanitized here? We don't want to be passing around untrusted data, especially not if it's touching the filesystem.
post '/create' do
  begin
    # Want
    @trial = Trial.create(
      params[:parser][:file_upload][:tempfile], 
      params[:user].values,
      params[:device].values
    )

    set_match_filtered_data

    erb :trial
  rescue Exception => e
    redirect '/trials?error=creation'
  end
end

# # TODO
# # - Can you add a comment here to explain what's going on? We spent a few minutes looking at it and couldn't figure it out.
def set_match_filtered_data
#   files = Dir.glob(File.join('public/uploads', "*"))
#   files.delete(@trial.file_name)

#   match = if @trial.parser.is_data_combined?
#     files.select { |f| @trial.file_name == f.gsub('-s.', '-c.') }.first
#   else
#     files.select { |f| @trial.file_name == f.gsub('-c.', '-s.') }.first
#   end

#   @match_filtered_data = if match
#     parser = Parser.new(File.read(match))
#     parser.filtered_data
#   end
end
