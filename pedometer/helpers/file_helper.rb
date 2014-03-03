require './models/user.rb'
require './models/device.rb'

class FileHelper

  def self.generate_file_name(user, device)
    "#{user.gender}-#{user.height}-#{user.stride}_" + 
    "#{device.rate}-#{device.method}-#{device.steps}-" + 
    "#{device.trial.to_s.gsub(/\s+/, '')}-#{device.format[0]}"
  end

  def self.parse_file_name(file_name = '--_----')
    u, d = file_name.split('/').last.split('_').collect { |i| i.split('-') }
    {user: {gender: u[0], height: u[1], stride: u[2]}, 
    device: {rate: d[0], method: d[1], steps: d[2], trial: d[3]}}
  end

end