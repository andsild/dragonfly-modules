IS_WINDOWS = True

def lineJuggle_logic(n1, n2, operation, linePrefix):
    upper_line=min(n1,n2)
    lower_line=max(n1,n2)
    if linePrefix == "+":
        upper_line, lower_line = lower_line, upper_line

    if upper_line==0:
        upper_line="."
    else:
        upper_line=linePrefix+str(upper_line)
    if lower_line==0:
        lower_line="."
    else:
        lower_line=linePrefix+str(lower_line)

    start=":"
    if not IS_WINDOWS:
        start= start + "silent "
    return start + str(lower_line) + "," + str(upper_line) + operation
