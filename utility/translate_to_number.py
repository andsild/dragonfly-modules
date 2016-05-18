from itertools import tee, izip

def pairwise(iterable):
    """ https://docs.python.org/2/library/itertools.html
    s -> (s0,s1), (s1,s2), (s2, s3), ...
    """
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)


scales_to_word = {}
tens_to_word = {}
unit_to_word = {}
units = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
        "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
        "sixteen", "seventeen", "eighteen", "nineteen",
      ]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
scales = ["hundred", "thousand", "ten thousand", "hundred thousand", "million", "ten million"]

for index, word in enumerate(units):    unit_to_word[word] = str(index)
for index, word in enumerate(tens):     tens_to_word[word] = str((index+2) * 10)
for index, word in enumerate(scales):   scales_to_word[word] =  ''.join("0" for _ in range(index+2))

def translate_word_to_number(text):
    print scales_to_word
    res = ""
    text_as_list = text.split()

    is_numerical = lambda words: words.isdigit() or words in unit_to_word or words in scales_to_word or words in tens_to_word

    for words,next in pairwise(text_as_list):
        words = words.lower()
        if not is_numerical(words):
            continue

        if not is_numerical(next):
            if words.isdigit():
                res += words
            elif words in scales_to_word:
                res += "1" + scales_to_word[words]
            elif words in unit_to_word:
                res += unit_to_word[words]
            elif words in tens_to_word:
                res += scales_to_word[words]
            continue

        if words.isdigit():
            res += words
        elif words in unit_to_word:
            res += unit_to_word[words]
        elif words in tens_to_word:
            if next in scales_to_word or next.isdigit():
                res += scales_to_word[words][0]
        elif words in scales_to_word:
            res += scales_to_word[words]
    return res