DROP TABLE metrics;
DROP TABLE tweets;
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


SELECT t0.id, t1.id
FROM metrics t0
LEFT OUTER JOIN metrics t1
ON (t0.id = t1.id AND t0.created_at < t1.created_at)
WHERE t1.id IS NULL
;