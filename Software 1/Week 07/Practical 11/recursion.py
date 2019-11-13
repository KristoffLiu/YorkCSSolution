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
    return int(number[0]) + sum_digits_str(number[1:len(number)])

    #if it is a int
def sum_digits(number):
    return int(str(number)[0]) + sum_digits(int(str(number)[1:len(number)]))

#Exercise 03
def rec_sum(numbers):
    return int(numbers[0]) + rec_sum(numbers[1:len(numbers)])

#Exercise 04
def iselfish(word):
    if word[0:2].isnumeric():
        