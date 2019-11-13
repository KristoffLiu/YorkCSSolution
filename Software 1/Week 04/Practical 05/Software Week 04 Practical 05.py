# Exercise 06
def scalar_product(scalar,vector):
    result = []
    for v in vector:
        result.append(v*scalar)
    return result

def vector_addition(vector1, vector2):
    result = []
    if len(vector1)==len(vector2):
        for index in range(len(vector1)):
            result.append(vector1[index] + vector2[index])
    else:
        print("vector1 and vector2 don’t have the same dimension, you should print an error message.")
    return result

# Paraphrase

def scalar_product_p(scalar,vector):
    return [vector[i] * scalar for i in range(len(vector))]

def vector_addition_p(vector1, vector2):
    if len(vector1) == len(vector2):
        return [vector1[i] + vector2[i] for i in range(len(vector1))]
    else:
        print("vector1 and vector2 don’t have the same dimension, you should print an error message.")

print(vector_addition([2,5,7],[2,5,7]))