require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include ViewHelper

get '/uploads' do
  @error = "A #{params[:error]} error has occurred." if params[:error]

  uploads = Upload.all
  @analyzers = []
  uploads.each do |upload|
    parser    = Parser.run(File.read(upload.file_path))
    processor = Processor.run(parser.parsed_data)
    user      = upload.user
    trial     = upload.trial
    analyzer  = Analyzer.run(processor.filtered_data, user, trial)

    @analyzers << analyzer
  end

  erb :uploads
end

get '/upload/*' do |file_path|
  upload = Upload.find(file_path)

  parser    = Parser.run(File.read(file_path))
  processor = Processor.run(parser.parsed_data)
  user      = upload.user
  trial     = upload.trial
  @analyzer = Analyzer.run(processor.filtered_data, user, trial)

  erb :upload
end

# TODO
# - Is file sanitized here? We don't want to be passing around untrusted data, especially not if it's touching the filesystem.
post '/create' do
  begin
    parser    = Parser.run(File.read(params[:data][:tempfile]))
    processor = Processor.run(parser.parsed_data)
    user      = User.new(*params[:user].values)
    trial     = Trial.new(*params[:trial].values)
    @analyzer = Analyzer.run(processor.filtered_data, user, trial)

    Upload.create(params[:data][:tempfile], user, trial)

    redirect '/uploads'
  rescue Exception => e
    redirect '/uploads?error=creation'
  end
end