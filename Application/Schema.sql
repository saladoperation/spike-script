-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE tweets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    tweet_id TEXT NOT NULL UNIQUE
);
CREATE TABLE metrics (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    retweet_count INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX metrics_created_at_index ON metrics (created_at);
