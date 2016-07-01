# Based on the work done by the creators of the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Modifications by: Tony Grosinger and Anders Sildnes
#
# Licensed under LGPL

class FormatTypes:
    camelCase = 1
    pascalCase = 2
    snakeCase = 3
    squash = 4
    upperCase = 5
    lowerCase = 6
    dashify = 7
    dotify = 8
    spokenForm = 9
    sentenceCase = 10

def format_text_logic(text, formatType):
    def format_camel_case(text):
        return text[0] + ''.join([word[0].upper() + word[1:].lower() for word in text[1:]])


    def format_pascal_case(text_list):
        return ''.join(word[0].upper() + word[1:].lower() for word in text_list)


    def format_snake_case(text_list):
        newText = ""
        for word in text_list:
            if newText != "" and newText[-1:].isalnum() and word[-1:].isalnum():
                word = "_" + word  # Adds underscores between normal words.
            newText += word.lower()
        return newText


    def format_dashify(text_list):
        newText = ""
        for word in text_list:
            if newText != "" and newText[-1:].isalnum() and word[-1:].isalnum():
                word = "-" + word  # Adds dashes between normal words.
            newText += word
        return newText


    def format_dotify(text):
        newText = ""
        words = text.strip()
        for word in words:
            if newText != "" and newText[-1:].isalnum() and word[-1:].isalnum():
                word = "." + word  # Adds dashes between normal words.
            newText += word
        return newText

    def format_squash(text):
        return ''.join(text)

    def format_sentence_case(text):
        newText = []
        words = text.strip()
        for word in words:
            if newText == []:
                newText.append(word.title())
            else:
                newText.append(word.lower())
        return " ".join(newText)


    def format_upper_case(text_list):
        return ' '.join(x.upper() for x in text_list)


    def format_lower_case(text_list):
        return ' '.join(x.lower() for x in text_list)


    def format_spoken_form(text_list):
        newText = ""
        for word in text_list:
            if newText != "":
                word = " " + word
            newText += word
        return newText



    class FormatTypes:
        camelCase = 1
        pascalCase = 2
        snakeCase = 3
        squash = 4
        upperCase = 5
        lowerCase = 6
        dashify = 7
        dotify = 8
        spokenForm = 9
        sentenceCase = 10

    FORMAT_TYPES_MAP = {
        FormatTypes.sentenceCase: format_sentence_case,
        FormatTypes.camelCase: format_camel_case,
        FormatTypes.pascalCase: format_pascal_case,
        FormatTypes.snakeCase: format_snake_case,
        FormatTypes.squash: format_squash,
        FormatTypes.upperCase: format_upper_case,
        FormatTypes.lowerCase: format_lower_case,
        FormatTypes.dashify: format_dashify,
        FormatTypes.dotify: format_dotify,
        FormatTypes.spokenForm: format_spoken_form,
    }

    result = text.split()
    for value in formatType: # there can be multiple types
        result = FORMAT_TYPES_MAP[value](result)
    return result
