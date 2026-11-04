# Transak purchase-session compatibility evidence

## Authority and review status

Expectations in `rust/tests/transak.rs` and its child modules are source-derived,
not captured from replacement output. Independent human fixture, CORS, URI-parser,
and numeric-expiry review remains pending. No Haskell comparison runner, real
Transak session, production credential, or public node is used.

Behavioral authorities:

- `src/Transak.hs`: `TransakConfig`, endpoint selection, `emptyAccessToken`,
  `refreshAccessToken`, `getAccessToken`, `createWidgetUrl`, and
  `tryCreateWidgetUrl` define the outbound HTTP contract and refresh coordination.
- `src/Proxy.hs`: `resolveTransakUserIp`, `postTransakOnRamp`, and
  `optionsTransakOnRamp` define validation order, IP formatting, and public errors.
- `app/Main.hs`: `transakCorsPolicy` defines exact origins, no credentials,
  POST/OPTIONS, Content-Type, Vary Origin, optional Origin, and ignored failures.
- `src/Internationalization/En.hs`: error wording and shared error classifications.
- Aeson's URI instance uses an absolute URI parser. The locally installed Aeson
  and Network.URI interfaces were inspected as source evidence, not executed as
  an output oracle. `rust/src/transak/uri.rs` explicitly checks RFC 3986 components
  without URL normalization; full Network.URI lexer equivalence is not claimed.

## Fixed outbound HTTP fixtures

Tests use a synthetic partner key/secret, account `[7; 32]`, a clock initially at
POSIX second 1000, and a local HTTP server with queued responses.

Refresh POST:

```json
{"apiKey":"synthetic-key"}
```

Headers are `accept: application/json`, `content-type: application/json`,
`api-secret: synthetic-secret`, and `x-api-key: synthetic-key`.
A normal controlled response is:

```json
{"data":{"accessToken":"synthetic-token","expiresAt":2000}}
```

Session POST:

```json
{"widgetParams":{"apiKey":"synthetic-key","cryptoCurrencyCode":"CCD","referrerDomain":"wallet.example","walletAddress":"<base58 account [7; 32]>","disableWalletAddressForm":true}}
```

Headers are `accept: application/json`, `content-type: application/json`,
`access-token: synthetic-token`, `x-api-key: synthetic-key`, and the selected
`x-user-ip`. The API secret is not sent to the session endpoint.

A controlled session response returns:

```json
{"data":{"widgetUrl":"https://EXAMPLE.invalid/buy?session=a%2fb#Step"}}
```

The wallet response is HTTP 200 with `widgetUrl` at the top level. Letter case,
percent escapes, and the fragment remain unchanged. Tests also cover opaque
absolute URIs, IPv6 authorities, query delimiters, malformed escapes, relative
references, invalid ports/brackets, and invalid characters.

Production and staging host/path selection is asserted without contacting those
hosts. Configuration uses typed flags/environment values instead of the legacy
application configuration file, as required by the PRD's startup model. The
mapping and forwarded-header trust boundary are documented in `rust/README.md`.

## Public request and error fixtures

| Case | HTTP | Body |
| --- | --- | --- |
| Disabled POST or OPTIONS | 404 | `{"error":2,"errorMessage":"Not found"}` |
| Missing address | 400 | `{"error":1,"errorMessage":"Parsing input failed: 'address' parameter is required to be an account address"}` |
| Invalid address | 400 | `{"error":1,"errorMessage":"Parsing input failed: 'address' is not a valid account address"}` |
| Missing, invalid UTF-8, or blank first forwarded IP; missing peer address | 500 | `{"error":0,"errorMessage":"Internal server error"}` |
| Refresh/session status, decode, URI, or transport failure | 502 | `{"error":0,"errorMessage":"Invalid response from upstream server."}` |
| Whole operation exceeds ten seconds | 504 | `{"error":0,"errorMessage":"Timeout occurred waiting for response from upstream server."}` |

Validation is disabled state, then the first `address` query value, then user IP,
then upstream operations. Request bodies are ignored. Forwarded mode trims only
the first comma-separated entry and does not validate it as an IP address.
Remote mode ignores forwarding headers; IPv6 uses all eight lowercase groups
without leading zeroes or compression. A real TCP HTTP test checks peer capture
and the shared response metadata.

## Refresh and failure policy

- Startup does not contact Transak. The shared token starts empty with expiry 0.
- Refresh occurs only when `now + 60 > expiry`, not at equality. Fractional expiry
  is parsed without floating-point rounding, with POSIXTime's picosecond resolution.
- One async lock serializes token access/refresh. The old token is not replaced
  until refresh and decoding succeed. Failure, cancellation, and timeout release
  the lock and retain that value. Session requests do not retain the lock.
- A session 401 calls the same getter and retries once. It **does not force a new
  refresh** when the cached token is still valid. Tests cover both reuse of the
  same token and observing a token refreshed by another concurrent request.
- Other statuses and malformed successful responses do not retry. The overall
  ten-second limit includes refresh-lock waits, HTTP reads, and the optional retry.
- Queued local responses and explicit wall-clock changes control concurrency;
  paused Tokio time checks timeouts without ten-second real waits. Credential,
  token, upstream body, and widget URL contents are not logged in failure paths.

## Browser-origin fixtures

The same tests run with public CORS enabled and disabled. Allowed Transak origins
receive their exact origin and `Vary: Origin`, never `*` or credentials. Valid
preflights allow POST/OPTIONS and Content-Type only and do not create sessions.
Disallowed method/header/origin checks omit permission headers but fall through
to the OPTIONS handler, which returns HTTP 200 with empty text when enabled.
Missing/disallowed origins on POST still create sessions, as required by
`corsIgnoreFailures = True`. A disabled Transak route returns HTTP 404 even for
public-CORS-enabled preflights. No policy is promoted to server authentication.

## Remaining acceptance

Independent fixture/CORS/header-byte/URI/expiry-parser review, observed CI, Task 01
acceptance, and the shared HTTP compatibility review remain open. Extreme numeric
exponents, duplicate/nesting boundaries, and unusual URI forms require explicit
review rather than assumptions of complete Aeson/Network.URI equivalence. The
existing shared JSON nesting bound and indexed/scientific-number normalization
also apply to complete upstream JSON decoding. Controlled tests are not approval
to enable production credentials or create real purchase sessions.
