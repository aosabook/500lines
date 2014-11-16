class User

  GENDER      = ['male', 'female']
  MULTIPLIERS = {'female' => 0.413, 'male' => 0.415}
  AVERAGES    = {'female' => 70.0,  'male' => 78.0}
  
  attr_reader :gender, :height, :stride

  def initialize(gender = nil, height = nil, stride = nil)
    @gender = gender.to_s.downcase if GENDER.include? gender.to_s.downcase
    # @height = height.to_f if height.to_f > 0
    # @stride = (stride.to_f > 0) ? stride.to_f : calculate_stride
    @height = float_or_default(height, nil, 0)
    @stride = float_or_default(stride, calculate_stride, 0)
  end

private

  def float_or_default(val, default, greater_than_val)
    float = val.to_f
    if (float.to_s == val.to_s || float.to_s.gsub('.0', '') == val.to_s)
      (float > greater_than_val) ? float : default
    else
      default
    end
  end

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