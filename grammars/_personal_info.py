from dragonfly import AppContext,Grammar, Dictation, IntegerRef, Text, Choice, Key, Function, MappingRule

from dragonflymodules.config import *

mapping = {
    "may name": Text(My_name),
    "may surname": Text(My_surname),
    "may full name": Text(My_full_name),
    "may address": Text(My_address),
    "may email": Text(My_email),
    "may work email": Text(My_work_email),
    "may student email": Text(My_student_email),
    "may [phone] number": Text(My_student_email),
}

class PersonalInfoRule(MappingRule):
    mapping = mapping
    extras = [
        Dictation('text'),
        IntegerRef('n', 1, 999),
        IntegerRef('n2', 1, 999),
    ]
    defaults = {
        "n": 1,  # Default repeat count.
    }

rules = PersonalInfoRule()