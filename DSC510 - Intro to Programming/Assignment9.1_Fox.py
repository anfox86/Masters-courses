# File: Assignment9.1_Fox
# Name: Andrea Fox
# Assignment 9.1 - Use open API to open data for end user
import json
import requests

def get_joke():
    url = ("https://api.chucknorris.io/jokes/random")
    response = requests.request("GET", url)
    joke_dict = json.loads(response.text)
    joke = joke_dict.get("value")
    print(joke)
def main():
    print("Welcome to my page of Chuck Norris jokes!")
    ask_user = input('Do you want to see a Chuck Norris joke? Y/N: \n')
    ask_user = ask_user.upper()
    while ask_user != 'N':
        if ask_user == 'Y':
            get_joke()
            ask_user = input('Do you want to see another one? Y/N: \n')
            ask_user = ask_user.upper()
        else:
            ask_user = input('I did not understand you.  Please enter  Y or N: \n')
            ask_user = ask_user.upper()
main()






