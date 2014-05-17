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
