class User

  GENDER = ['male', 'female']
  SYSTEM = ['metric', 'imperial']

  attr_reader :system, :gender, :height, :stride, :threshold

  # TODO: Allow threshold to be passed in
  def initialize(params = {})
    params = {} unless params.kind_of? Hash

    system_params = params[:system].to_s.downcase
    gender_params = params[:gender].to_s.downcase
    stride_params = params[:stride].to_f.round(2)

    @system = (SYSTEM.include? system_params) ? system_params : 'metric'
    @gender = gender_params if GENDER.include? gender_params
    @height = params[:height]
    @stride = (stride_params > 30) ? stride_params : calculate_stride
    @threshold = 0.2
  end

  # TODO: calculate stride based on measurement system
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
    end.round(2)
  end

end