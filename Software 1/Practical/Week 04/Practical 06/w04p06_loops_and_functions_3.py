import practical_6
# Exercise 2
def split_text_by(text, separators):
    result = []
    word = ""
    for i in text:
        if i in separators:
            if word != "":
                result.append(word)
                word = ""
            else:
                word = word + i

    if word != "":
        result.append(word)
    return result

print(split_text_by("hello, here is kristoff, are you ok?",","))