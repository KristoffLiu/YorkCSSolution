import math
MINUS_INFINITE = - math.inf

def maxvalue(btree):
    if type(btree) == int or type(btree) == float:
        return btree
    if not btree:
        return MINUS_INFINITE
    elif 