class User

  GENDER = ['male', 'female']

  attr_reader :gender, :height, :stride

  def initialize(params = {})
    params = {} unless params.kind_of? Hash

    gender_params = params[:gender].to_s.downcase
    stride_params = params[:stride].to_f.round(2)
    height_params = params[:height].to_f.round(2)

    @gender = gender_params if GENDER.include? gender_params
    @height = height_params if height_params > 0
    @stride = (stride_params > 0) ? stride_params : calculate_stride
  end

  # avg: 74 cm
  # avg male: 78 cm
  # avg female: 70 cm
  # male height * .415
  # female height * .413
  def calculate_stride
    if @height && @gender
      (@gender == 'male') ? 0.415 * @height : 0.413 * @height
    elsif @height
      @height * 0.414
    elsif @gender
      (@gender == 'male') ? 78 : 70
    else
      74
    end.round(2)
  end

end