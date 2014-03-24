require_relative '../models/user'
require_relative '../models/parser'
require_relative '../models/device'

class FileHelper

  def self.generate_file_name(parser, user, device)
    "public/uploads/#{user.gender}-#{user.height}-#{user.stride}_" +
    "#{device.rate}-#{device.method}-#{device.steps}-" +
    "#{device.trial.to_s.gsub(/\s+/, '')}-#{parser.format[0]}.txt"
  end

  def self.parse_file_name(file_name = '--_----')
    u, d = file_name.split('/').last.split('_').collect { |i| i.split('-') }
    [u, d[0...-1]]
  end

end