require 'sinatra'
require 'sinatra/jbuilder'
require './models/pedometer.rb'

get '/metrics' do
  begin
    user = User.new(params[:user])
    device_data = DeviceData.new(params[:data])
    @pedometer = Pedometer.new(device_data, user)
    @pedometer.measure    
    [200, jbuilder(:index)]
  rescue Exception => e
    [400, e.message]
  end
end