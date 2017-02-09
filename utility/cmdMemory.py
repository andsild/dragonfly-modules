#TODO: one tricky case, what to do if part of the history was expressions such as
# "big alpha"? (answer: use text_translate when it is finished)
def get_backspaces_for_commands(history, count=1):
    func = lambda li: len(li) - 1 + sum(len(word) for word in li)
    return sum(map(func, history[-count:]))