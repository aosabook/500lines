require_relative '../models/user'
require_relative '../models/parser'

class FileHelper

  def self.generate_file_name(user, parser)
    "#{user.gender}-#{user.height}-#{user.stride}_" +
    "#{parser.device.rate}-#{parser.device.method}-#{parser.device.steps}-" +
    "#{parser.device.trial.to_s.gsub(/\s+/, '')}-#{parser.format[0]}"
  end

  def self.parse_file_name(file_name = '--_----')
    u, d = file_name.split('/').last.split('_').collect { |i| i.split('-') }
    [u, d[0...-1]]
  end

end