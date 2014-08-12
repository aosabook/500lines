MISSING = object()

class Map(object):
    def __init__(self, attrs):
        self.attrs = attrs
        self.next_maps = {}

    def get_index(self, fieldname):
        return self.attrs.get(fieldname, -1)

    def next_map(self, fieldname):
        assert fieldname not in self.attrs
        if fieldname in self.next_maps:
            return self.next_maps[fieldname]
        attrs = self.attrs.copy()
        attrs[fieldname] = len(attrs)
        result = self.next_maps[fieldname] = Map(attrs)
        return result

EMPTY_MAP = Map({})

class Base(object):
    """ The base class that all of the object model classes inherit from. """

    def __init__(self, cls):
        """ Every object has a class. """
        self.cls = cls

    def read_attr(self, fieldname):
        """ read field 'fieldname' out of the object """
        result = self._read_dict(fieldname)
        if result is not MISSING:
            return result
        result = self.cls._read_from_class(fieldname)
        if hasattr(result, "__get__"):
            return _make_boundmethod(result, self, self)
        if result is not MISSING:
            return result
        meth = self.cls._read_from_class("__getattr__")
        if meth is MISSING:
            raise AttributeError(fieldname)
        return meth(self, fieldname)

    def write_attr(self, fieldname, value):
        """ write field 'fieldname' into the object """
        meth = self.cls._read_from_class("__setattr__")
        return meth(self, fieldname, value)

    def isinstance(self, cls):
        """ return True if the object is an instance of class cls """
        return self.cls.issubclass(cls)

    def send(self, methname, *args):
        """ send message 'methname' with arguments `args` to object """
        meth = self.read_attr(methname)
        return meth(*args)

    def _read_dict(self, fieldname):
        """ read an field 'fieldname' out of the object's dict """
        return MISSING

    def _write_dict(self, fieldname, value):
        """ write a field 'fieldname' into the object's dict """
        raise AttributeError

def __setattr__OBJECT(self, fieldname, value):
    self._write_dict(fieldname, value)


class BaseWithDict(Base):
    def __init__(self, cls, fields):
        Base.__init__(self, cls)
        self._fields = fields

    def _read_dict(self, fieldname):
        return self._fields.get(fieldname, MISSING)

    def _write_dict(self, fieldname, value):
        self._fields[fieldname] = value


class Instance(Base):
    """Instance of a user-defined class. """

    def __init__(self, cls):
        assert isinstance(cls, Class)
        Base.__init__(self, cls)
        self.map = EMPTY_MAP
        self.storage = []

    def _read_dict(self, fieldname):
        index = self.map.get_index(fieldname)
        if index == -1:
            return MISSING
        return self.storage[index]

    def _write_dict(self, fieldname, value):
        index = self.map.get_index(fieldname)
        if index != -1:
            self.storage[index] = value
        else:
            new_map = self.map.next_map(fieldname)
            self.storage.append(value)
            self.map = new_map


def _make_boundmethod(meth, cls, self):
    return meth.__get__(self, cls)

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
OBJECT = Class("object", None, {"__setattr__": __setattr__OBJECT}, None)
# TYPE is a subclass of OBJECT
TYPE = Class("type", OBJECT, {}, None)
# TYPE is an instance of itself
TYPE.cls = TYPE
# OBJECT is an instance of TYPE
OBJECT.cls = TYPE
