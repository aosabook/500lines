class User

  GENDER = ['male', 'female']

  attr_reader :gender, :height, :stride

  # TODO: 
  # - Named params ruby 2.0
  def initialize(gender = nil, height = nil, stride = nil)
    params = {} unless params.kind_of? Hash

    gender_param = gender.to_s.downcase
    height_param = height.to_f
    stride_param = stride.to_f

    @gender = gender_param if GENDER.include? gender_param
    @height = height_param if height_param > 0
    @stride = (stride_param > 0) ? stride_param : calculate_stride
  end

  # avg: 74 cm
  # avg male: 78 cm
  # avg female: 70 cm
  # male height * .415
  # female height * .413

  # TODO: 
  # - Declare constants for multipliers and strides
  # - No @ on instance variables
  # - Diff between private and protected?
  # - Check all places where .method_name is called at the end of if statements
private

  def calculate_stride
    if @gender && @height
      (@gender == 'male') ? 0.415 * @height : 0.413 * @height
    elsif @height
      @height * 0.414
    elsif @gender
      (@gender == 'male') ? 78 : 70
    else
      74
    end
  end

end