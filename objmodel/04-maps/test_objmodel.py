from objmodel import Class, Instance, TYPE, OBJECT

# copied from the previous version
def test_isinstance():
    # Python code
    class A(object):
        pass
    class B(A):
        pass
    b = B()
    assert isinstance(b, B)
    assert isinstance(b, A)
    assert isinstance(b, object)
    assert not isinstance(b, type)

    # Object model code
    A = Class("A", OBJECT, {}, TYPE)
    B = Class("B", A, {}, TYPE)
    b = Instance(B)
    assert b.isinstance(B)
    assert b.isinstance(A)
    assert b.isinstance(OBJECT)
    assert not b.isinstance(TYPE)

# copied from the previous version
def test_read_write_field():
    # Python code
    class A(object):
        pass
    obj = A()
    obj.a = 1
    assert obj.a == 1

    obj.b = 5
    assert obj.a == 1
    assert obj.b == 5

    obj.a = 2
    assert obj.a == 2
    assert obj.b == 5

    # Object model code
    A = Class("A", OBJECT, {}, TYPE)
    obj = Instance(A)
    obj.write_field("a", 1)
    assert obj.read_field("a") == 1

    obj.write_field("b", 5)
    assert obj.read_field("a") == 1
    assert obj.read_field("b") == 5

    obj.write_field("a", 2)
    assert obj.read_field("a") == 2
    assert obj.read_field("b") == 5

# copied from the previous version
def test_send_simple():
    # Python code
    class A(object):
        def f(self):
            return self.x + 1
    obj = A()
    obj.x = 1
    assert obj.f() == 2

    class B(A):
        pass
    obj = B()
    obj.x = 1
    assert obj.f() == 2 # works on subclass too

    # Object model code
    def f(self):
        return self.read_field("x") + 1
    A = Class("A", OBJECT, {"f": f}, TYPE)
    obj = Instance(A)
    obj.write_field("x", 1)
    assert obj.send("f") == 2

    B = Class("B", A, {}, TYPE)
    obj = Instance(B)
    obj.write_field("x", 2)
    assert obj.send("f") == 3

# copied from the previous version
def test_send_subclassing_and_arguments():
    # Python code
    class A(object):
        def g(self, arg):
            return self.x + arg
    obj = A()
    obj.x = 1
    assert obj.g(4) == 5

    class B(A):
        def g(self, arg):
            return self.x + arg * 2
    obj = B()
    obj.x = 4
    assert obj.g(4) == 12

    # Object model code
    def g_A(self, arg):
        return self.read_field("x") + arg
    A = Class("A", OBJECT, {"g": g_A}, TYPE)
    obj = Instance(A)
    obj.write_field("x", 1)
    assert obj.send("g", 4) == 5

    def g_B(self, arg):
        return self.read_field("x") + arg * 2
    B = Class("B", A, {"g": g_B}, TYPE)
    obj = Instance(B)
    obj.write_field("x", 4)
    assert obj.send("g", 4) == 12


# copied from the previous version
def test_bound_method():
    # Python code
    class A(object):
        def f(self, a):
            return self.x + a + 1
    obj = A()
    obj.x = 2
    m = obj.f
    assert m(4) == 7

    class B(A):
        pass
    obj = B()
    obj.x = 1
    m = obj.f
    assert m(10) == 12 # works on subclass too

    # Object model code
    def f(self, a):
        return self.read_field("x") + a + 1
    A = Class("A", OBJECT, {"f": f}, TYPE)
    obj = Instance(A)
    obj.write_field("x", 2)
    m = obj.read_field("f")
    assert m(4) == 7

    B = Class("B", A, {}, TYPE)
    obj = Instance(B)
    obj.write_field("x", 1)
    m = obj.read_field("f")
    assert m(10) == 12

def test_getattr():
    # Python code
    class A(object):
        def __getattr__(self, name):
            if name == "fahrenheit":
                return self.celsius * 9. / 5. + 32
            raise AttributeError(name)

        def __setattr__(self, name, value):
            if name == "fahrenheit":
                self.celsius = (value - 32) * 5. / 9.
            else:
                # call the base implementation
                object.__setattr__(self, name, value)
    obj = A()
    obj.celsius = 30
    assert obj.fahrenheit == 86
    obj.celsius = 40
    assert obj.fahrenheit == 104

    obj.fahrenheit = 86
    assert obj.celsius == 30
    assert obj.fahrenheit == 86

    # Object model code
    def __getattr__(self, name):
        if name == "fahrenheit":
            return self.read_field("celsius") * 9. / 5. + 32
        raise AttributeError(name)
    def __setattr__(self, name, value):
        if name == "fahrenheit":
            self.write_field("celsius", (value - 32) * 5. / 9.)
        else:
            # call the base implementation
            OBJECT.read_field("__setattr__")(self, name, value)

    A = Class("A", OBJECT, {"__getattr__": __getattr__, "__setattr__": __setattr__}, TYPE)
    obj = Instance(A)
    obj.write_field("celsius", 30)
    assert obj.read_field("fahrenheit") == 86
    obj.write_field("celsius", 40)
    assert obj.read_field("fahrenheit") == 104
    obj.write_field("fahrenheit", 86)
    assert obj.read_field("celsius") == 30
    assert obj.read_field("fahrenheit") == 86

def test_get():
    # Python code
    class FahrenheitGetter(object):
        def __get__(self, inst, cls):
            return inst.celsius * 9. / 5. + 32

    class A(object):
        fahrenheit = FahrenheitGetter()
    obj = A()
    obj.celsius = 30
    assert obj.fahrenheit == 86

    # Object model code
    class FahrenheitGetter(object):
        def __get__(self, inst, cls):
            return inst.read_field("celsius") * 9. / 5. + 32

    def __getattr__(self, name):
        if name == "fahrenheit":
            return self.read_field("celsius") * 9. / 5. + 32
        raise AttributeError(name)

    A = Class("A", OBJECT, {"fahrenheit": FahrenheitGetter()}, TYPE)
    obj = Instance(A)
    obj.write_field("celsius", 30)
    assert obj.read_field("fahrenheit") == 86


def test_maps():
    # white box test inspecting the implementation
    Point = Class("Point", OBJECT, {}, TYPE)
    p1 = Instance(Point)
    p1.write_field("x", 1)
    p1.write_field("y", 2)

    p2 = Instance(Point)
    p2.write_field("x", 5)
    p2.write_field("y", 6)
    assert p1.map is p2.map
    assert p1.storage == [1, 2]
    assert p2.storage == [5, 6]

    p1.write_field("x", -1)
    p1.write_field("y", -2)
    assert p1.map is p2.map
    assert p1.storage == [-1, -2]
