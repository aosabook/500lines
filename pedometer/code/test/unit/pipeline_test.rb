require 'test/unit'
require_relative '../../models/pipeline'

class PipelineTest < Test::Unit::TestCase

  def test_new_combined_data
    file_path = 'test/data/female-167-70_bagwalk1-100-10.txt'
    user = User.new
    trial = Trial.new('foobar1', 100)
    pipeline = Pipeline.run(File.read(file_path), user, trial)

    assert_equal user, pipeline.user
    assert_equal trial, pipeline.trial
    assert pipeline.parser
    assert pipeline.processor
    assert pipeline.analyzer

    assert_equal 12, pipeline.analyzer.steps
    assert_equal 888.0, pipeline.analyzer.distance
    assert_equal 8, pipeline.analyzer.time
  end

  def test_new_separated_data
    file_path = 'test/data/female-167-70_bagwalk2-100-10.txt'
    user = User.new
    trial = Trial.new('foobar1', 100)
    pipeline = Pipeline.run(File.read(file_path), user, trial)

    assert_equal user, pipeline.user
    assert_equal trial, pipeline.trial
    assert pipeline.parser
    assert pipeline.processor
    assert pipeline.analyzer

    assert_equal 12, pipeline.analyzer.steps
    assert_equal 888.0, pipeline.analyzer.distance
    assert_equal 9, pipeline.analyzer.time
  end

end
