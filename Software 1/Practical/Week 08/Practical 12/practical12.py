import math
#exercise 01
def wildcard_pattern(wc_str,wc_char):
    #return [wildcard_pattern(wc_str,wc_char,False) for i in range(int(math.pow(2,wc_str.count(wc_char))))]
    if wc_str:
        if wc_str[0] == wc_char:
            return ["0" + wildcard_pattern(wc_str[1:],wc_char)] + ["1" + wildcard_pattern(wc_str[1:],wc_char)]
        else:
            return wc_str[0] + wildcard_pattern(wc_str[1:],wc_char)


def test():
    return {"1","2"}


print(wildcard_pattern("1?0?","?"))