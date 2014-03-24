class User

  GENDER = ['male', 'female']
  AVERAGES = {'female' => 70, 'male' => 78}
  MULTIPLIERS = {'female' => 0.413, 'male' => 0.415}

  attr_reader :gender, :height, :stride

  # TODO: Named params ruby 2.0 useful here?
  def initialize(gender = nil, height = nil, stride = nil)
    params = {} unless params.kind_of? Hash

    gender_param = gender.to_s.downcase
    height_param = height.to_f
    stride_param = stride.to_f

    @gender = gender_param if GENDER.include? gender_param
    @height = height_param if height_param > 0
    @stride = (stride_param > 0) ? stride_param : calculate_stride
  end

private

  def calculate_stride
    if gender && height
      MULTIPLIERS[@gender] * height
    elsif height
      height * (MULTIPLIERS.values.reduce(:+) / MULTIPLIERS.size)
    elsif gender
      AVERAGES[gender]
    else
      AVERAGES.values.reduce(:+) / AVERAGES.size
    end
  end

end