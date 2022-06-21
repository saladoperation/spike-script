ALTER TABLE metrics ADD COLUMN tweet_id UUID NOT NULL;
CREATE INDEX metrics_tweet_id_index ON metrics (tweet_id);
ALTER TABLE metrics ADD CONSTRAINT metrics_ref_tweet_id FOREIGN KEY (tweet_id) REFERENCES tweets (id) ON DELETE NO ACTION;
