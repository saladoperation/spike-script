DROP TABLE metrics;
DROP TABLE tweets;
CREATE TABLE tweets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    retweet_count INT DEFAULT 0 NOT NULL
);