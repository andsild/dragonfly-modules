IS_WINDOWS = True

def lineJuggle_logic(n1, n2, operation, linePrefix):
    delete_to_mark=min(n1,n2)
    delete_from_mark=max(n1,n2)

    if linePrefix == "+":
        delete_to_mark, delete_from_mark = delete_from_mark, delete_to_mark
        delete_to_mark=delete_to_mark-delete_from_mark # lower_line is now offset relative to upper_line
    else:
        delete_to_mark=delete_from_mark-delete_to_mark

    if n1 == n2:
        delete_to_mark = "0"

    delete_to_mark='+'+str(delete_to_mark)
    delete_from_mark=linePrefix+str(delete_from_mark)

    start=":"
    if not IS_WINDOWS:
        start= start + "silent "
    return start + str(delete_from_mark) + "," + str(delete_to_mark) + operation
