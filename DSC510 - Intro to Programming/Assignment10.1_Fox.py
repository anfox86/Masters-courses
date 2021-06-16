# File: Assignment10.1_Fox
# Name: Andrea Fox
# Assignment 10.1 - Cash Register

import locale
class CashRegister:
    def __init__(self):
        self.total = 0
        self.count = 0

    def add_item(self, price):
        self.total += price
        self.count += 1

    def get_total(self):
        return self.total

    def get_count(self):
        return self.count
def main():
    my_cart = CashRegister()
    print("Welcome Andrea's Cash Register Application")
    while True:
        num_items = int(input("Enter 1 to add item to cart and 0 to quit: "))
        if num_items == 1:
            price = float(input("Enter price of the item: "))
            my_cart.add_item(price)
        elif num_items == 0:
            break
        else:
            print("Sorry, invalid input...Please try again")
    locale.setlocale(locale.LC_ALL, 'en_US')
    print("Total items in cart:", my_cart.get_count())
    print("Total price:", end=' ')
    print(locale.currency(my_cart.get_total()))
main()
