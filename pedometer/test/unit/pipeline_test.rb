require 'test/unit'
require_relative '../../models/pipeline'
require_relative '../../models/upload'

class PipelineTest < Test::Unit::TestCase

  def test_new_combined_data
    file_path = 'test/data/female-167-70_1-100-10-bagwalk.txt'
    upload = Upload.find(file_path)
    pipeline = Pipeline.run(upload)

    assert_equal upload, pipeline.upload
    assert pipeline.parser
    assert pipeline.processor
    assert pipeline.analyzer

    assert_equal 12, pipeline.analyzer.steps
    assert_equal 840.0, pipeline.analyzer.distance
    assert_equal 8, pipeline.analyzer.time
  end

  def test_new_separated_data
    file_path = 'test/data/female-167-70_2-100-10-bagwalk.txt'
    upload = Upload.find(file_path)
    pipeline = Pipeline.run(upload)
    
    assert_equal upload, pipeline.upload
    assert pipeline.parser
    assert pipeline.processor
    assert pipeline.analyzer

    assert_equal 12, pipeline.analyzer.steps
    assert_equal 840.0, pipeline.analyzer.distance
    assert_equal 9, pipeline.analyzer.time  
  end

end