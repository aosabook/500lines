# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'catechism/version'

Gem::Specification.new do |spec|
  spec.name          = 'catechism'
  spec.version       = Catechism::VERSION
  spec.authors       = ['Doc Ritezel and Sarah Mei']
  spec.email         = ['pair+doc+sarah@ministryofvelocity.com']
  spec.description   = 'An object-oriented testing framework'
  spec.summary       = 'Put your code through an object-oriented trial by fire.'
  spec.homepage      = 'https://github.com/aosabook/500lines/tree/master/catechism'
  spec.license       = 'MIT'

  spec.files         = `git ls-files`.split($/)
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^trials/})
  spec.require_paths = ['lib']
  spec.bindir        = 'bin'

  spec.add_dependency 'thor'

  spec.add_development_dependency 'bundler', '~> 1.3'
  spec.add_development_dependency 'gem-release'
  spec.add_development_dependency 'guard-shell'
end
