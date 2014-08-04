require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

get '/uploads' do
  @uploads = Upload.all
  @error = "A #{params[:error]} error has occurred." if params[:error]

  erb :uploads
end

get '/upload/*' do |file_name|
  @upload = Upload.find(file_name)
  
  erb :upload
end

# TODO
# - Is file sanitized here? We don't want to be passing around untrusted data, especially not if it's touching the filesystem.
post '/create' do
  begin
    @upload = Upload.create(
      params[:processor][:file_upload][:tempfile], 
      params[:user].values,
      params[:device].values
    )

    erb :upload
  rescue Exception => e
    redirect '/uploads?error=creation'
  end
end
