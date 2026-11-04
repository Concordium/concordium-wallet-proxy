-- This is the Persistent GTURecipient table owned by the proxy.
CREATE TABLE IF NOT EXISTS g_t_u_recipient (
    id BIGSERIAL PRIMARY KEY,
    account BYTEA NOT NULL,
    transaction BYTEA NOT NULL,
    CONSTRAINT unique_account UNIQUE (account)
);
