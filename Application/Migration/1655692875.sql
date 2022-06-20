DROP TABLE tweets;
CREATE TABLE tweets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    tweet_id TEXT NOT NULL UNIQUE
);