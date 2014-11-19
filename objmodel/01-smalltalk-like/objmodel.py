MISSING = object()

class Base(object):
    """ The base class that all of the object model classes inherit from. """

    def __init__(self, cls):
        """ Every object has a class. """
        self.cls = cls

    def read_attr(self, fieldname):
        """ read field 'fieldname' out of the object """
        return self._read_dict(fieldname)

    def write_attr(self, fieldname, value):
        """ write field 'fieldname' into the object """
        self._write_dict(fieldname, value)

    def isinstance(self, cls):
        """ return True if the object is an instance of class cls """
        return self.cls.issubclass(cls)

    def callmethod(self, methname, *args):
        """ call method 'methname' with arguments 'args' on object """
        meth = self.cls._read_from_class(methname)
        return meth(self, *args)

    def _read_dict(self, fieldname):
        """ read an field 'fieldname' out of the object's dict """
        return MISSING

    def _write_dict(self, fieldname, value):
        """ write a field 'fieldname' into the object's dict """
        raise AttributeError


class BaseWithDict(Base):
    def __init__(self, cls, fields):
        Base.__init__(self, cls)
        self._fields = fields

    def _read_dict(self, fieldname):
        return self._fields.get(fieldname, MISSING)

    def _write_dict(self, fieldname, value):
        self._fields[fieldname] = value


class Instance(BaseWithDict):
    """Instance of a user-defined class. """

    def __init__(self, cls):
        assert isinstance(cls, Class)
        BaseWithDict.__init__(self, cls, {})


class Class(BaseWithDict):
    """ A User-defined class. """

    def __init__(self, name, base_class, fields, metaclass):
        BaseWithDict.__init__(self, metaclass, fields)
        self.name = name
        self.base_class = base_class

    def mro(self):
        """ compute the mro (method resolution order) of the class """
        if self.base_class is None:
            return [self]
        else:
            return [self] + self.base_class.mro()

    def issubclass(self, cls):
        """ is self a subclass of cls? """
        return cls in self.mro()

    def _read_from_class(self, methname):
        for cls in self.mro():
            if methname in cls._fields:
                return cls._fields[methname]
        return MISSING

# set up the base hierarchy like in Python (the ObjVLisp model)
# the ultimate base class is OBJECT
OBJECT = Class(name="object", base_class=None, fields={}, metaclass=None)
# TYPE is a subclass of OBJECT
TYPE = Class(name="type", base_class=OBJECT, fields={}, metaclass=None)
# TYPE is an instance of itself
TYPE.cls = TYPE
# OBJECT is an instance of TYPE
OBJECT.cls = TYPE
