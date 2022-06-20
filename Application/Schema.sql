-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE tweets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    tweet_id TEXT NOT NULL
);
