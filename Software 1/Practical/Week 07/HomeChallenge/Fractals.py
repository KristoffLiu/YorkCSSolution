#Problem I
def Koch_Curve(turtle, length, degree):
    if degree == 0:
           #turtle must draw a segment of size length 
    else:
        length = length / 3
        degree = degree – 1
        korch_curve (turtle, length, degree) # First segment
        #turtle turn left 60
        korch_curve (turtle, length, degree) # Second segment
        #turtle turn right 120
        korch_curve (turtle, length, degree) # Third segment
        #turtle turn left 60   
        korch_curve (turtle, length, degree) # Fourth segment