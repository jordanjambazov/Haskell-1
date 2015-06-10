import re

from collections import defaultdict


def is_part_of_lang(word, lang_pattern):
    groups = []
    chars_in_word = []
    for group_name in re.findall(r"([a-z])\^n", lang_pattern):
        groups.append(group_name)
    chars_count = defaultdict(lambda: 0)
    for char in word:
        chars_count[char] += 1
        if char not in chars_in_word:
            chars_in_word.append(char)
    constructed_word = "".join([char * max(chars_count.values())
                                for char in chars_in_word])
    return word == constructed_word


def main():
    while True:
        try:
            print("Input:")
            lang_pattern = raw_input()
            word = raw_input()
            word_exists = is_part_of_lang(word, lang_pattern)
            print("Output:")
            print(("no", "yes")[word_exists])
            print("="*30)
        except KeyboardInterrupt:
            print("Bye")
            break


if __name__ == "__main__":
    main()
