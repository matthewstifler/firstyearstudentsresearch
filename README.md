#Local identity development for the students who come to study to Saint Petersburg, Russia

This project is dedicated to the place identity development for the urban newcomers: students from other cities. To perform this research, posts on social media are analyzed, then given topics regarding place identity (such as 'heritage' or 'sights') are found in the posts using word2vec.
This research can be roughly split into 3 main parts, data fetching and preparation, creating topics and getting similarities from them, building statistical models and making conclusions:

1. Data preparation
  1.1 Gather data from Vk.com SNS
  1.2 Process data, create two data frames, for students and for posts
  1.3 Process textual data
2. Creating topics
  2.1 Create word2vec model for later use
  2.2 Create LDA models, formulate topics for later research
  2.3 Calculate similarity between each post and each topic
  2.4 Turn continuous similarity into binary variables
3. Conclusions
  3.1 Create descriptive stats
  3.2 Summarise data for users and for periods
  3.3 Build statistical models
  3.4 Draw pretty pictures
