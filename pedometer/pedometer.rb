require 'sinatra'
require 'sinatra/jbuilder'
require './models/analyzer.rb'

get '/metrics' do
  begin
    device_data = DeviceData.new(params[:data])
    p device_data.filtered_data
    user = User.new(params[:user])
    
    @analyzer = Analyzer.new(device_data, user)
    @analyzer.measure    

    [200, jbuilder(:index)]
  rescue Exception => e
    [400, e.message]
  end
end