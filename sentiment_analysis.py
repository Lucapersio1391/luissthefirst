from tweepy import API
from tweepy import Cursor
from tweepy.streaming import StreamListener  #class that will allow to listen from the tweets based on some keywords or hashtags
from tweepy import OAuthHandler  #this class is going to be responsible for authenticating with my credentials
from tweepy import Stream 
from textblob import TextBlob
import twitter_credential
import numpy as np
import pandas as pd

#### TWITTER CLIENT #####
class TwitterClient():
    def __init__(self, twitter_user=None):
        self.auth = TwitterAuthenticator().authenticate_twitter_app()
        self.twitter_client = API(self.auth)

        self.twitter_user = twitter_user
    
    def get_twitter_client_api(self):
        return self.twitter_client

    def get_user_timeline_tweets(self, num_tweets):
        tweets = []
        for tweet in Cursor(self.twitter_client.user_timeline, id =self.twitter_user).items(num_tweets):
            tweets.append(tweet)
        return tweets


    def get_friend_list(self, num_friends):
        friend_list = []
        for friend in Cursor(self.twitter_client.friends, id=self.twitter_user).items(num_friends):
            friend_list.append(friend)
        return friend_list

    def get_home_timeline_tweets(self, num_tweets):
        home_timeline_tweets = []
        for tweet in Cursor(self.twitter_client.home_timeline, id=self.twitter_user).items(num_tweets):
            home_timeline_tweets.append(tweet)
        return home_timeline_tweets

        # This class handles twitter authentication and the connection to the Twitter streaming API
class TwitterAuthenticator():
    def authenticate_twitter_app(self):
        auth = OAuthHandler(twitter_credential.CONSUMER_KEY, twitter_credential.CONSUMER_SECRET)
        auth.set_access_token(twitter_credential.ACCESS_TOKEN, twitter_credential.ACCESS_TOKEN_SECRET)
        return auth

## TWITTER STREAMER ###
class TwitterStreamer():
    """
    Class for streaming and processing live tweets
    """
    def __init__(self):
        self.twitter_authenticator = TwitterAuthenticator()

    def stream_tweets(self, fechted_tweets_filename, hash_tag_list):
        listener = TwitterListener(fechted_tweets_filename)
        auth = self.twitter_authenticator.authenticate_twitter_app()
        stream = Stream(auth, listener)

        stream.filter(track = hash_tag_list)


# create a class that will allow to print the tweets

### TWITTER STREAM LISTENER #####

class TwitterListener(StreamListener):  # this class is going to inherit from StreamListener so that we can override StreamListener methods
    """
    Basic listener class that just prints received tweets to stdout
    """
    def __init__(self, fecthed_tweets_filename):

        self.fechted_tweets_filename = fecthed_tweets_filename

    def on_data(self, data):
        try:
            print(data)
            with open(self.fetched_tweets_filename, 'a') as tf:
                tf.write(data)
                return True
        except BaseException as e:
            print('Error on_data: %s' % str(e))
            return True
        

    def on_error(self, status):
        if status == 420:
            return False
        print(status)
 

class TweetAnalyzer():
    def tweets_to_data_frame(self, tweets):
        df = pd.DataFrame(data=[tweet.text for tweet in tweets], columns=['tweets'])

        df['id'] = np.array([tweet.id for tweet in tweets])
        df['len'] = np.array([len(tweet.text) for tweet in tweets])
        df['date'] = np.array([tweet.created_at for tweet in tweets])
        df['name'] = np.array([tweet.user.name for tweet in tweets])
        df['source'] = np.array([tweet.source for tweet in tweets])
        df['likes'] = np.array([tweet.favorite_count for tweet in tweets])
        df['retweets'] = np.array([tweet.retweet_count for tweet in tweets])

        return df  



# Let's create an object from StdOutListener class
if __name__ == "__main__":
    twitter_client = TwitterClient()
    tweet_analyzer = TweetAnalyzer()

    api = twitter_client.get_twitter_client_api()

    tweets = api.user_timeline(screen_name="gucci", count=200)

    #print(dir(tweets[0]))
    #print(tweets[0].retweet_count)

    df = tweet_analyzer.tweets_to_data_frame(tweets)
   # df['sentiment'] = np.array([tweet_analyzer.analyze_sentiment(tweet) for tweet in df["tweets"]])
    print(df['name'].tail(10))
