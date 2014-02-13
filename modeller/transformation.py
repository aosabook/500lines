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

def make_perspective(fov, aspect, near, far):
    fov = fov * math.pi / 180;
    f = 1/tan(fov/2);
    persp_mat = numpy.zeros((4, 4))
    persp_mat[0][0] = f/aspect;
    persp_mat[0][1] = 0;
    persp_mat[0][2] = 0;
    persp_mat[0][3] = 0;

    persp_mat[1][0] = 0;
    persp_mat[1][1] = f;
    persp_mat[1][2] = 0;
    persp_mat[1][3] = 0;

    persp_mat[2][0] = 0;
    persp_mat[2][1] = 0;
    persp_mat[2][2] = (far+near)/(near-far);
    persp_mat[2][3] = (2*far*near)/(near-far);

    persp_mat[3][0] = 0;
    persp_mat[3][1] = 0;
    persp_mat[3][2] = -1;
    persp_mat[3][3] = 0;

    return numpy.transpose(persp_mat)
