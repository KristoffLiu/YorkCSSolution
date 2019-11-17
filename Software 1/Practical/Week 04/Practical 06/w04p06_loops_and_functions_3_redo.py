import practical_6
#Exercise 01
sample_text = ""
def split_text(sample_text):
    result = []
    word = ""
    for char in sample_text:
        if char != " ":
            word += char
        else:
            result.append(word)
            word = ""
    result.append(word)
    print(result)

#Exercise 02
def split_text_by(sample_text,splitstring):
    result = []
    word = ""
    for char in sample_text:
        if char in splitstring:
            result.append(word)
            word = ""
        else:
            word += char
    result.append(word)
    print(result)

def exercise_02():
    sample_text = "As Python's creator, I'd like to say a few words about its origins."
    split_text_by(sample_text, ",'.")

#Exercise 03
def getWordsFrequency(text):
    words = text.split()
    output = []
    for word in words:
        for index in range(len(output)):
            if output[index][0] == word:
                output[index][1] += 1
                break
        else:
            output.append([word,1])
    return output

def exercise_03():
    print(getWordsFrequency(practical_6.sample_text))

#Exercise 04
def flatten(list_2D):
    output = []
    for lists in list_2D:
        for l in lists:
            output.append(l)
    print(output)

def exercise_04():
    flatten([[1,2],[],[3,4,5,6],[7],[8,9]])

#Exercise 05
def rasterise(list_1D,width):
    print([[list_1D[i:i+width] for i in range(0,len(list_1D),width)]] if len(list_1D)%width == 0 else None)

def exercise_05():
    rasterise([1,2,3,4,5,6,7,8],4) 

#Exercise 06
def sum_column():

def exercise_06():
    table = [[1,2,3],          [4,5,6],          [7,8,9]] 