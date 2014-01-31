import numpy

def translation(displacement):
  t = numpy.identity(4)
  t[3, 0] = displacement[0]
  t[3, 1] = displacement[1]
  t[3, 2] = displacement[2]
  return t

def scaling(scale):
  s = numpy.identity(4)
  s[0, 0] = scale[0]
  s[1, 1] = scale[1]
  s[2, 2] = scale[2]
  return s
