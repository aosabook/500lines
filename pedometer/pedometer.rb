require 'sinatra'

Dir['./models/*', './helpers/*'].each {|file| require_relative file }

include FileUtils::Verbose

get '/trials' do
  @error = "A #{params[:error]} error has occurred." if params[:error]

  @trials = Trial.all.map do |trial|
    { file_name: trial.file_name, analyzer: trial.analyzer }
  end

  erb :trials
end

get '/trial/*' do
  @trial = Trial.find(params[:splat].first)
  @match_filtered_data = Trial.find_matching_filtered_data(@trial)
  
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
    @match_filtered_data = Trial.find_matching_filtered_data(@trial)

    erb :trial
  rescue Exception => e
    redirect '/trials?error=creation'
  end
end
