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

#print(wildcard_pattern("1?0?","?"))

def allwords(digits,keypad = []):
    if len(digits) == 1:
        return keypad[digits]
    elif type(digits) == list:
        a = digits.pop(0)
        return 
    else:
        return set([char +   for char in allwords(digits[0])]) 


t9_keypad = { ’2’:[’a’,’b’,’c’], ’3’:[’d’,’e’,’f’],
              ’4’:[’g’,’h’,’i’], ’5’:[’j’,’k’,’l’],
              ’6’:[’m’,’n’,’o’], ’7’:[’p’,’q’,’r’,’s’],
              ’8’:[’t’,’u’,’v’], ’9’:[’w’,’x’,’y’,’z’]  }

print(allwords("24",t9_keypad))