#File: Assignment3.1_Fox
#Name: Andrea Fox
#Assignment 3.1
#Purpose: Allow user to place an order for fiber optic cable. Calculate the cost based on feet given as well as offer bulk discount

print('Welcome to the Fiber Optic Installation Company Page!')
companyname = input('What is your company name?')
fiberoptic = input('How many feet of fiber optic cable do you need?')

discount_1 = 0.80 * int(fiberoptic)
discount_2 = 0.70 * int(fiberoptic)
discount_3 = 0.50 * int(fiberoptic)
regular_price = 0.87 * int(fiberoptic)

def bulk_cost(fiberoptic):
    if int(fiberoptic)>100:
        print(discount_1)
    elif int(fiberoptic)>250:
        print(discount_2)
    elif int(fiberoptic)>500:
        print(discount_3)
    else:
        print(regular_price)

print('The total cost for', companyname, 'is')
bulk_cost(fiberoptic)
