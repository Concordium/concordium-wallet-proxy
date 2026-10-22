-- TEST ONLY. Subset of Proxy.hs's persistLowerCase declarations:
-- Summary sql=summaries and Entry sql=ati. This is not a production migration.
-- Extend with the relevant external tables when later vertical slices need them.
CREATE TABLE summaries (
    id BIGSERIAL PRIMARY KEY,
    block BYTEA NOT NULL,
    timestamp BIGINT NOT NULL,
    height BIGINT NOT NULL,
    summary JSONB NOT NULL
);
CREATE TABLE ati (
    id BIGSERIAL PRIMARY KEY,
    account BYTEA NOT NULL,
    summary BIGINT NOT NULL REFERENCES summaries(id)
);
CREATE INDEX ati_account_summary ON ati(account, summary);

-- AccountPublicKeyBinding uses BYTEA wrappers and Word8's SqlInt32 mapping.
-- The legacy declaration has no uniqueness constraint or discovery index.
CREATE TABLE account_public_key_bindings (
    id BIGSERIAL PRIMARY KEY,
    address BYTEA NOT NULL,
    public_key BYTEA NOT NULL,
    credential_index INTEGER NOT NULL,
    key_index INTEGER NOT NULL,
    is_simple_account BOOLEAN NOT NULL
);
