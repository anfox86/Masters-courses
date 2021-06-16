#File: Assignment8.1_Fox
#Name: Andrea Fox
#Assignment 8.1

import string
#Opening the file, closing file
def main():
    word_count = dict()
    gba_file = open('gettysburg.txt', 'r')
    for line in gba_file:
        process_line(line, word_count)
    userfile = input('Enter file name')
    process_file(word_count, userfile)

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
#Sort and write to file
def process_file(word_count, userfile):
    gba_copy_file = userfile + '.txt'
    userfile = open(gba_copy_file, 'w')
    word_list = list()
    for key, val in list(word_count.items()):
        word_list.append((val,key))
    word_list.sort(reverse = True)
    header = ('Length of the dictionary: ' + str(len(word_list)))
    userfile.write(header)
    userfile.write('\n')
    userfile.write("{0:13} {1}".format('Word', 'Count'))
    userfile.write('\n')
    userfile.write('\n---------------------------------')
    userfile.write('\n')
    for key, val in word_list:
        userfile.write("{0:15} {1}".format(val, key))
        userfile.write('\n')

if __name__ == '__main__':
    main()
