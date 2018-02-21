import csv, random, tweepy
from time import sleep

#enter the corresponding information from your Twitter application:
CONSUMER_KEY = '' #keep the quotes 
CONSUMER_SECRET = ''
ACCESS_KEY = ''
ACCESS_SECRET = ''

auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_KEY, ACCESS_SECRET)
api = tweepy.API(auth)

with open('BlackCards.csv', 'r') as f:
    reader = csv.reader(f)
    black_list = list(reader)

with open('WhiteCards.csv', 'r') as g:
    reader = csv.reader(g)
    white_list = list(reader)

while True:
    black_card = random.choice(black_list)
    white_card = random.choice(white_list)
    message = black_card[0] + white_card[0] + black_card[1]
    if len(message) < 280:
        api.update_status(status = message[0:279])
        print(message)
        print(len(message))
        sleep(7200) #2 hours between tweets
    else:
        sleep(1)
        
