#File: Assignment3.1_Fox
#Name: Andrea Fox
#Assignment 3.1
#Purpose: Allow user to place an order for fiber optic cable. Calculate the cost based on feet given as well as offer bulk discount

print('Welcome to the Fiber Optic Installation Company Page!')
companyname = input('What is your company name?')
fiberoptic = input('How many feet of fiber optic cable do you need?') #Need measurement to estimate cost

print("The total cost for the fiber optic installtion for ", end = "")
print(companyname)
print("is")

int_fiberoptic = int(fiberoptic)

s = int_fiberoptic
w = 0.87
x = 0.80
y = 0.70
z = 0.50

if s > 100:
    print(s*x)
elif s > 250:
    print(s*y)
elif s > 500:
    print(s*z)
else:
    print(s*w)






