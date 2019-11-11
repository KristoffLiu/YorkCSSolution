def product_polynomial(P,Q):
    
    return;

def eval_polynomial(poly,x):
    total_value = 0
    count = 0
    for individual in poly:
        count += 1
        total_value += (x^count) * individual
    print(total_value)
    return;

def add_polynomial(P,Q):
    print([P[i]+Q[i] for i in range(5)])

def product_polynomial(P,Q):
    X = []
    for p in P:
        Y = []
        for q in Q:
            Y.append(p * q)
        X.append(Y)
    return

product_polynomial([1,2,3,4,5],[1,2,3,4,5])
