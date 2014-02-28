class LanguageError(Exception):
    pass

class Instance:
    def __init__(self, cls, dct=None):
        assert cls is None or isinstance(cls, Class)
        self.cls = cls
        if dct is None:
            dct = {}
        self.dct = dct

    def read_field(self, fieldname):
        if fieldname in self.dct:
            return self.dct[fieldname]
        return self.cls._read_from_class(fieldname, self)

    def write_field(self, fieldname, value):
        self.dct[fieldname] = value

    def isinstance(self, cls):
        return self.cls.issubclass(cls)

    def send(self, methname, *args):
        meth = self.read_field(methname)
        return meth(*args)

def _make_boundmethod(meth, self):
    def bound(*args):
        return meth(self, *args)
    return bound

class Class(Instance):
    def __init__(self, name, base_class, dct, metaclass):
        Instance.__init__(self, metaclass, dct)
        self.name = name
        self.base_class = base_class

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
                if callable(result):
                    return _make_boundmethod(result, obj)
                return result
        raise LanguageError("method %s not found" % methname)

# set up the base hierarchy like in Python (the ObjVLisp model)
# the ultimate base class is OBJECT
OBJECT = Class("object", None, {}, None)
# TYPE is a subclass of OBJECT
TYPE = Class("type", OBJECT, {}, None)
# TYPE is an instance of itself
TYPE.cls = TYPE
# OBJECT is an instance of TYPE
OBJECT.cls = TYPE
