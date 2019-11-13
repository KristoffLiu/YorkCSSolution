# Exercise 0
def get_words_frequencies(text):
    results = [] #初始化一个空列表，用来装载答案
    words = text.split() #将text字符串处理，把词语都装进一个列表中
    for word in words:
        is_exist = False #初始默认该单词不存在
        if len(results) == 0: #如果列表是空的
            pass
        else: #如果不是空的
            for result in results:
                if result[0] == word: #如果单词已在results中，
                    result[1] += 1 #则给出现频次加1
                    is_exist = True #把这个boolean调成True
                    break; #跳出本次循环
        if is_exist == False: #如果单词不在results中
            results.append([word,1]) #则给出现频次加1
    print(results)

get_words_frequencies("a a a a a a a a hello here is kristoff hello here is kristoff hello here is kristoff hello here is kristoff here is kristoff")