CREATE TABLE metrics (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    retweet_count INT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX metrics_created_at_index ON metrics (created_at);
