#File: Assignment7.1_Fox
#Name: Andrea Fox
#Assignment 7.1

import string
#Opening the file, cleaning file up,
def main():
    word_count = dict()
    gba_file = open('gettysburg.txt', 'r')
    for line in gba_file:
        process_line(line, word_count)
    pretty_print(word_count)

#Created list, removed punctuation, and made all letters lower case
def process_line(line, word_count):
    line = line.translate(str.maketrans('','',string.punctuation))
    line = line.lower()
    words = line.split()
    for word in words:
        add_word(word, word_count)
#Add count to dictionary
def add_word(word, word_count):
    if word not in word_count:
        word_count[word] = 1
    else:
        word_count[word] += 1
#Sort and print
def pretty_print(word_count):
    word_list = list()
    for key, val in list(word_count.items()):
        word_list.append((val,key))
    word_list.sort(reverse = True)
    header = ('Length of the dictionary: ' + str(len(word_list)))
    print(header)
    print("{0:13} {1}".format('Word', 'Count'))
    print('\n---------------------------------')
    for key, val in word_list:
        print("{0:15} {1}".format(val, key))



if __name__ == '__main__':
    main()


