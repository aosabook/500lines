require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

get '/trials' do
  @trials = Trial.all
  @error = "A #{params[:error]} error has occurred." if params[:error]

  erb :trials
end

get '/trial/*' do |file_name|
  @trial = Trial.find(file_name)
  
  erb :trial
end

# TODO
# - Is file sanitized here? We don't want to be passing around untrusted data, especially not if it's touching the filesystem.
post '/create' do
  begin
    @trial = Trial.create(
      params[:parser][:file_upload][:tempfile], 
      params[:user].values,
      params[:device].values
    )

    erb :trial
  rescue Exception => e
    redirect '/trials?error=creation'
  end
end
