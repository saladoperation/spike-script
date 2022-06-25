-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE tweets (
    id TEXT PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    retweet_count INT DEFAULT 0 NOT NULL
);
