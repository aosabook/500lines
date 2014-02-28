class LanguageError(Exception):
    pass

class Map(object):
    def __init__(self, attrs):
        self.attrs = attrs
        self.next_maps = {}

    def get_index(self, fieldname):
        return self.attrs.get(fieldname, -1)

    def next_map(self, fieldname):
        if fieldname in self.next_maps:
            return self.next_maps[fieldname]
        attrs = self.attrs.copy()
        assert fieldname not in attrs
        attrs[fieldname] = len(attrs)
        result = self.next_maps[fieldname] = Map(attrs)
        return result

EMPTY_MAP = Map({})

class Base(object):
    def __init__(self, cls):
        self.cls = cls

    def read_field(self, fieldname):
        raise NotImplementedError("abstract base class")

    def write_field(self, fieldname, value):
        raise NotImplementedError("abstract base class")

    def isinstance(self, cls):
        return self.cls.issubclass(cls)

    def send(self, methname, *args):
        meth = self.read_field(methname)
        return meth(*args)

class Instance(Base):
    def __init__(self, cls):
        assert cls is None or isinstance(cls, Class)
        self.cls = cls
        self.map = EMPTY_MAP
        self.storage = []

    def read_field(self, fieldname):
        index = self.map.get_index(fieldname)
        if index != -1:
            return self.storage[index]
        try:
            return self.cls._read_from_class(fieldname, self)
        except AttributeError, orig_error:
            # not found on class
            pass
        # try special method __getattr__
        try:
            meth = self.cls._read_from_class("__getattr__", self)
        except AttributeError:
            raise orig_error # get the right error message
        return meth(fieldname)

    def write_field(self, fieldname, value):
        meth = self.cls._read_from_class("__setattr__", self)
        return meth(fieldname, value)


def _make_boundmethod(meth, cls, self):
    return meth.__get__(self, cls)

class Class(Base):
    def __init__(self, name, base_class, dct, metaclass):
        Base.__init__(self, metaclass)
        self.dct = dct
        self.name = name
        self.base_class = base_class

    def read_field(self, fieldname):
        if fieldname in self.dct:
            return self.dct[fieldname]
        raise AttributeError("attribute %s not found" % fieldname)

    def write_field(self, fieldname, value):
        self.dct[fieldname] = value

    def mro(self):
        """ compute the mro (method resolution order) of the class """
        if self.base_class is None:
            return [self]
        else:
            return [self] + self.base_class.mro()

    def issubclass(self, cls):
        return cls in self.mro()

    def _read_from_class(self, methname, obj):
        for cls in self.mro():
            if methname in cls.dct:
                result = cls.dct[methname]
                if hasattr(result, "__get__"):
                    return _make_boundmethod(result, self, obj)
                return result
        raise AttributeError("method %s not found" % methname)


def __setattr__OBJECT(self, fieldname, value):
    index = self.map.get_index(fieldname)
    if index != -1:
        self.storage[index] = value
    else:
        new_map = self.map.next_map(fieldname)
        self.storage.append(value)
        self.map = new_map

# set up the base hierarchy like in Python (the ObjVLisp model)
# the ultimate base class is OBJECT
OBJECT = Class("object", None, {"__setattr__": __setattr__OBJECT}, None)
# TYPE is a subclass of OBJECT
TYPE = Class("type", OBJECT, {}, None)
# TYPE is an instance of itself
TYPE.cls = TYPE
# OBJECT is an instance of TYPE
OBJECT.cls = TYPE
