def print_my_name():
    print "Anders Sildnes"

print "Lorem dolor sit et amet"

def count_to_ten(count):
     if count >= 10:
         return 1
     return count_to_ten(count + 1)
 

def fibonacci(count):
    if count < 2:
        return count

    return fibonacci(count - 1) + fibonacci(count - 2)



print count_to_ten(0)
print_my_name()
print fibonacci(8)
