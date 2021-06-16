#File: Assignment6.1_Fox
#Name: Andrea Fox
#Assignment 6.1 Temperatures

temperatures = []
def main():
    while True:
        temp = input("Enter a temperature")
        if temp == 'done':
            break
        temperatures.append(temp)
        print(temperatures)

print(main())
print('The high temperature is', max(temperatures))
print('The low temperature is', min(temperatures))
print('The number of temperatures given is', len(temperatures))