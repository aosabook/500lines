class User

  GENDER = ['male', 'female']
  SYSTEM = ['metric', 'imperial']

  attr_accessor :gender, :height, :stride, :system

  def initialize(params)
    params ||= {}

    @system = (SYSTEM.include? params[:system].to_s.downcase) ? params[:system].to_s.downcase : 'metric'
    @gender = params[:gender].to_s.downcase if GENDER.include? params[:gender].to_s.downcase
    @height = params[:height]
    @stride = params[:stride] || calculate_stride
  end


  # TODO calculate stride based on measurement system
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