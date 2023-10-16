
class Foo:

    def __init__ (self):
        self.__x = 100

    @property
    def value (self):
        return self.__x

    @value.setter
    def value (self, x):
        self.__x = x * 100

f = Foo()

print(f.value)

f.value = 99

print(f.value)


