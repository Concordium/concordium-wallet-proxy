-- TEST ONLY. Health-query subset of Proxy.hs's persistLowerCase declarations:
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
