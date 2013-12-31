require 'sinatra'
require 'sinatra/jbuilder'
require './models/pedometer.rb'

get '/metrics' do
  begin
    @pedometer = Pedometer.new(params[:data])
    @pedometer.measure    
    [200, jbuilder(:index)]
  rescue Exception => e
    [400, e.message]
  end
end