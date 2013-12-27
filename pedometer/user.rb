class User

  attr_accessor :gender, :height, :stride, :system

def initialize(params)
  params ||= {}

  @gender = params[:gender]
  @height = params[:height]
  @stride = params[:stride] || calculate_stride
  @system = params[:system] || 'metric'
end

def calculate_stride
  # avg: 74 cm
  # avg male: 78 cm
  # avg female: 70 cm
  # male height * .415
  # female height * .413

  if @height && @gender
    multiplier = (@gender == 'male') ? 0.415 : 0.413
    @height * multiplier    
  elsif @height
    @height * 0.415
  elsif @gender
    (@gender == 'male') ? 78 : 70
  else
    74
  end
end

end