def exercise_1():
    a_word = "hello world"
    f = open("exo1.txt",'a')
    f.write(a_word)
    f.close()

def save_list2file(sentences, filename):
    f = open(filename,"w")

    f.close()
