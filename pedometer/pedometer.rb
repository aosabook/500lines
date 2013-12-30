require 'sinatra'
require 'sinatra/jbuilder'
require './models/pedometer.rb'


# TODO: Return success and failure ([status (Fixnum), response]), 
# don't blow up on failures at the model level
get '/metrics' do
  data = params[:data] || ""
  @pedometer = Pedometer.new(data)
  @pedometer.measure
  
  jbuilder :index
end