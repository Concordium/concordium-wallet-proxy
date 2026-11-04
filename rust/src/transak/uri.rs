//! This module checks absolute widget URIs without normalizing their returned text.
//! It follows RFC 3986 scheme, authority, path, query, and fragment character classes.

pub(super) fn valid(text: &str) -> bool {
    let Some((scheme, rest)) = text.split_once(':') else {
        return false;
    };
    if !scheme
        .as_bytes()
        .first()
        .is_some_and(u8::is_ascii_alphabetic)
        || !scheme
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || b"+-.".contains(&byte))
    {
        return false;
    }
    let (main, fragment) = rest
        .split_once('#')
        .map_or((rest, None), |(main, fragment)| (main, Some(fragment)));
    if fragment.is_some_and(|fragment| !component(fragment, b":@/?")) {
        return false;
    }
    let (hierarchy, query) = main
        .split_once('?')
        .map_or((main, None), |(main, query)| (main, Some(query)));
    if query.is_some_and(|query| !component(query, b":@/?")) {
        return false;
    }
    if let Some(rest) = hierarchy.strip_prefix("//") {
        let (host, path) = rest
            .split_once('/')
            .map_or((rest, ""), |(host, path)| (host, path));
        authority(host) && component(path, b":@/")
    } else {
        component(hierarchy, b":@/")
    }
}
fn authority(text: &str) -> bool {
    let host = if let Some((user, host)) = text.rsplit_once('@') {
        if !component(user, b":") {
            return false;
        }
        host
    } else {
        text
    };
    if let Some(literal) = host.strip_prefix('[') {
        let Some((literal, rest)) = literal.split_once(']') else {
            return false;
        };
        let port = rest.is_empty()
            || rest
                .strip_prefix(':')
                .is_some_and(|port| port.bytes().all(|byte| byte.is_ascii_digit()));
        let ip = if let Some(future) = literal.strip_prefix(['v', 'V']) {
            future.split_once('.').is_some_and(|(version, address)| {
                !version.is_empty()
                    && version.bytes().all(|byte| byte.is_ascii_hexdigit())
                    && !address.is_empty()
                    && !address.contains('%')
                    && component(address, b":")
            })
        } else {
            literal.parse::<std::net::Ipv6Addr>().is_ok()
        };
        ip && port
    } else {
        let (name, port) = host
            .split_once(':')
            .map_or((host, None), |(name, port)| (name, Some(port)));
        component(name, b"")
            && port.is_none_or(|port| port.bytes().all(|byte| byte.is_ascii_digit()))
    }
}
fn component(text: &str, extra: &[u8]) -> bool {
    let mut bytes = text.bytes();
    while let Some(byte) = bytes.next() {
        if byte == b'%' {
            if !bytes.next().is_some_and(|byte| byte.is_ascii_hexdigit())
                || !bytes.next().is_some_and(|byte| byte.is_ascii_hexdigit())
            {
                return false;
            }
        } else if !byte.is_ascii_alphanumeric()
            && !b"-._~!$&'()*+,;=".contains(&byte)
            && !extra.contains(&byte)
        {
            return false;
        }
    }
    true
}
