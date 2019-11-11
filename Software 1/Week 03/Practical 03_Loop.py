#Exercise 1
def Exercise1_1():
    inputstr = input("enter a sentence:")
    output = ""
    for char in inputstr:
        if char != " ":
            output += char
    print(output)        
    return;

def Exercise1_2():
    inputstr = input("enter a sentence: ")
    output = ""
    isfirstletterofaword = True
    for char in inputstr:
        if char != " ":
            if isfirstletterofaword == True:
                output += char.upper()
                isfirstletterofaword = False
            else:
                output += char.lower()
        else:
            isfirstletterofaword = True            
    print(output)        
    return;


def Exercise1_2_Paraphrase():
    inputstr = input("enter a sentence: ")           
    print(inputstr.title())        
    return;

def Exercise1_2_Paraphrase2():
    inputstr = input("enter a sentence: ")
    output = ""
    isfirstletterofaword = True
    for char in inputstr:
        if char != " ":
            if isfirstletterofaword == True:
                output += char.upper()
                isfirstletterofaword = False
            else:
                output += char.lower()
        else:
            isfirstletterofaword = True            
    print(output)        
    return;

def Exercise1_3():
    inputstr = input("enter a sentence: ")
    output = []
    text = ""
    for char in inputstr:
        if char != " ":
            text += char
        else:
            output.append(text)
            text = ""
    output.append(text)
    print(output)        
    return;

#Exercise 2
def Exercise2_1():
    inputstr = input("enter a series of numbers: ")
    output = []
    inttext = ""
    for char in inputstr:
        if char != " ":
            inttext += char
        else:
            intitem = int(inttext)
            if( intitem % 2 == 0):
                output.append(intitem)
            inttext = ""
    print(output)
    print(len(output))        
    return;

def Exercise2_2():
    inputstr = input("enter a series of numbers: ")
    output = []
    inttext = ""
    for char in inputstr:
        if char != " ":
            inttext += char
        else:
            intitem = int(inttext)
            if( intitem % 2 == 0):
                output.append(intitem)
            inttext = ""
    print(output)
    for i in range(len(output)):
        str(output[i])+" " 
    print("there are",len(output),"even numbers:")       
    return;







Exercise1_2_Paraphrase()
