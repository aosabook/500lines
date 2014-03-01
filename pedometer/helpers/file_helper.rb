class FileHelper

  def self.generate_file_name(gender, height, stride, rate, method, steps, trial, format)
    trial = trial.to_s.gsub(/\s+/, '')
    format = format.to_s[0]
    "#{gender}-#{height}-#{stride}_#{rate}-#{method}-#{steps}-#{trial}-#{format}"
  end

  def self.parse_file_name(file_name = '--_----')
    user, device = file_name.split('_').collect { |i| i.split('-') }
    {user: {gender: user[0], height: user[1], stride: user[2]}, 
    device: {rate: device[0], method: device[1], steps: device[2], trial: device[3]}}
  end

end