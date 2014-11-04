require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include ViewHelper

get '/uploads' do
  @error = "A #{params[:error]} error has occurred." if params[:error]
  @pipelines = Upload.all.inject([]) do |a, upload|
    a << Pipeline.new(upload)
    a
  end

  erb :uploads
end

get '/upload/*' do |file_path|
  upload = Upload.find(file_path)
  @pipeline = Pipeline.new(upload)
  
  erb :upload
end

post '/create' do
  begin
    user = User.new(*params[:user].values)
    trial = Trial.new(*params[:trial].values)
    Upload.create(params[:data][:tempfile], user, trial)

    redirect '/uploads'
  rescue Exception => e
    redirect '/uploads?error=creation'
  end
end