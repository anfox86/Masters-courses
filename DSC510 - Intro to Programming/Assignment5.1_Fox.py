#File: Assignment5.1_Fox
#Name: Andrea Fox
#Assignment 5.1

def performCalculation():
    num1 = int(input('Select your first number'))
    num2 = int(input('Select your second number'))
    select_option = int(input('Enter a number: 1-4'))
    if select_option == 1:
        print(num1+num2)
    elif select_option== 2:
        print(num1-num2)
    elif select_option== 3:
        print(num1*num2)
    elif select_option== 4:
        print(num1/num2)
    else:
        print('Have a great day!')

def calculateAverage():
    input_int = input("Enter a list of numbers separated by space ")
    list = input_int.split()
    print("Calculating sum and average of numbers from input list")
    sum = 0
    for num in list:
        sum += int(num)
    print("Sum = ", sum, "Average =", sum/len(list))

operation = int(input("Which operation?"))
while (operation > 0):
    operation = int(input("Which operation?"))
    if operation == 1:
        print(performCalculation())
    elif operation == 2:
        print(calculateAverage())
    else:
        print("Have a nice day!")




