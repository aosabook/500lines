require_relative '../models/user'
require_relative '../models/device'

class FileHelper

  def self.generate_file_name(user, device)
    "#{user.gender}-#{user.height}-#{user.stride}_" + 
    "#{device.rate}-#{device.method}-#{device.steps}-" + 
    "#{device.trial.to_s.gsub(/\s+/, '')}-#{device.format[0]}"
  end

  def self.parse_file_name(file_name = '--_----')
    u, d = file_name.split('/').last.split('_').collect { |i| i.split('-') }
    [u, d[0...-1]]
  end

end