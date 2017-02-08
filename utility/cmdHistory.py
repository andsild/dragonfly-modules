def get_backspaces_for_commands(history, count=1):
    func = lambda li: len(li) - 1 + sum(len(word) for word in li)
    return sum(map(func, history[-count:]))