class Vector:
    #exercise 01
    def __init__(self,inputlist):
        self._vector = []
        _vector = inputlist
    
    #exercise 02
    def __str__(self):
        return "<" + str(self._vector).strip("[]") + ">"

    #exercise 03
    def dim(self):
        return len(self._vector)

    #exercise 04
    def get(self,index):
        return self._vector[index]
    
    def set(self,index,value):
        self._vector[index] = value

    def scalar_product(self, scalar):
        return [scalar * x for x in self._vector]

    #exercise 05
    def add(self, other_vector):
        if not isinstance(other_vector) == True and type(other_vector) == Vector:
            raise TypeError
        elif not self.dim() == other_vector.dim():
            raise ValueError
        else:
            return self.scalar_product(other_vector)

    #exercise 06
    def equals(self,other_vector):
        if not self.dim() == other_vector.dim():
            return False
        elif self == other_vector:
            return True
        else:
            for i in range(self.dim()):
                if self._vector[i] != other_vector._vector[i]:
                    return False
            else:
                return True

    #

    
