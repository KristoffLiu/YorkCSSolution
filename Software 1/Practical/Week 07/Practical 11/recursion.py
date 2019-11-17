#Exercise 01
def is_power(a,b):
    if a%b == 0:
        if a/b == 3:
            return True
        else:
            is_power(a/b,b)
    else:
        return False

#Exercise 02
    #if it is a string
def sum_digits_str(number):
    if len(number) == 1:
        return int(number)
    else:
        return int(number[0]) + sum_digits_str(number[1:len(number)])

    #if it is a int
def sum_digits(number):
    if len(str(number)) == 1:
        return number
    else:
        return int(str(number)[0]) + sum_digits(int(str(number)[1:len(str(number))]))

#Exercise 03
def rec_sum(numbers):
    return int(numbers[0]) + rec_sum(numbers[1:len(numbers)])

#Exercise 04
def iselfish(word):
    if word[0:3].isnumeric():
        if word[3] == "e":
            word[0] == "1"
        elif word[3] == "l":
            word[1] == "1"
        elif word[3] == "f":
            word[2] == "1"
        if word[0:3] == "111":
            return True
        elif len(word) > 4:
            return False
        else:
            return iselfish(word[0:3] + word[4:len(word)])
    else:
        return iselfish("000" + word)

def something_ish(pattern,word):
    if pattern:
        if word.count(pattern[0]) > 0:
            return something_ish(pattern[1:len(pattern)],word)
        else:
            False
    else:
        return True

#Exercise 05
def flatten(mlist):
    if mlist:
        if type(mlist[0]) == list:
            return flatten(mlist.pop(0)) + flatten(mlist)
        else:
            return mlist.pop(0) + flatten(mlist)
    else:
        return 0

#Exercise 06
def merge(sorted_listA,sorted_listB):
    if sorted_listA and sorted_listB:
        a = sorted_listA.pop(0)
        b = sorted_listB.pop(0)
        sortedpart = [a] + [b] if sorted_listA[0] > sorted_listB[0] else [b] + [a]
        return sortedpart + merge(sorted_listA,sorted_listB)
    else:
        return []

print(sum_digits_str("123"))