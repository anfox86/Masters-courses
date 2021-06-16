#File: Assignment2.1_FibertOpticCable.py
#Name: Andrea Fox
#Assignment 2.1
#Purpose: Allow user to put in an order for fiber optic cable to be installed, and give them a receipt for the cost.

print('Welcome to the Fiber Optic Installation Company Page!')
companyname = input('What is your company name?')
fibertoptic = input('How many feet of fiber optic cable do you need?') #Need measurement for amount of cable to estimate cost
x = 12 #Amount of cable needed
y = 0.87 #Cost per foot of fiber optic cable
print("Here is the total cost for the fiber optic cable installation for DSC510 T301 Company. Your 12ft of cable was multiplied by our price of $0.87 per foot. Your final total is, ", end = "")
print(x*y) #final receipt






