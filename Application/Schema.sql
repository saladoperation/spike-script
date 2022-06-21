-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE tweets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    tweet_id TEXT NOT NULL UNIQUE
);
CREATE TABLE metrics (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    retweet_count INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    tweet_id UUID NOT NULL
);
CREATE INDEX metrics_created_at_index ON metrics (created_at);
CREATE INDEX metrics_tweet_id_index ON metrics (tweet_id);
ALTER TABLE metrics ADD CONSTRAINT metrics_ref_tweet_id FOREIGN KEY (tweet_id) REFERENCES tweets (id) ON DELETE NO ACTION;
