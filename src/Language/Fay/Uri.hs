{-# Language EmptyDataDecls    #-}
{-# Language NoImplicitPrelude #-}

module Language.Fay.Uri where

import           Language.Fay.FFI
import           Language.Fay.Prelude

-- | Creation and conversion

-- Choose to make Uri an opaque data type. If the accessors on the
-- jsUri objects were properties and not functions we could have
-- defined Uri as a record instead.
data Uri
-- The Foreign declaration lets us use Uri in the FFI
instance Foreign Uri

-- To make Google Closure play nice we use [] instead of . for accessing properties.
currentUri :: Fay String
currentUri = ffi "window['location']['href']"

-- This assumes that Uri is defined globally, which it is by default.
newUri :: String -> Uri
newUri = ffi "new window['Uri'](%1)"

toString :: Uri -> String
toString = ffi "%1['toString']()"

-- If we ever want to pass a Uri back to JS we need to make sure we keep persistance internally.
clone :: Uri -> Uri
clone = ffi "%1['clone']()"

-- | Getters

-- All getters (except query! But lets be consistent) may return null if the value isn't set so we use
-- Language.FFI.Nullable here which converts null -> Null and String -> Nullable String.
-- Nullable is distinguished from Maybe to not break haskell compatibility.
-- If we want to decode null into Nullable (Nullable a) there is no way of
-- knowing if Null or Nullable Null is correct. This problem does not exist when
-- working with Maybe values in client server communication.
protocol :: Uri -> Nullable String
protocol = ffi "%1['protocol']()"

userInfo :: Uri -> Nullable String
userInfo = ffi "%1['userInfo']()"

host :: Uri -> Nullable String
host = ffi "%1['host']()"

port :: Uri -> Nullable String
port = ffi "%1['port']()"

path :: Uri -> Nullable String
path = ffi "%1['path']()"

query :: Uri -> Nullable String
query = ffi "%1['query']()"

anchor :: Uri -> Nullable String
anchor = ffi "%1['anchor']()"

-- | Other getters

queryParamValue :: String -> Uri -> String
queryParamValue = ffi "%2['getQueryParamValue'](%1)"

queryParamValues :: String -> Uri -> [String]
queryParamValues = ffi "%2['getQueryParamValues'](%1)"

-- | Setters

-- We could use Nullable here to combine the with* and remove* functions
-- but usage would be more verbose that way.

-- JsUri has clone() conveniently defined so we use it to get
-- persistence, otherwise our types would be `-> Fay Uri` which is of
-- course worse.

withProtocol :: String -> Uri -> Uri
withProtocol = ffi "%2['clone']()['setProtocol'](%1)"

withUserInfo :: String -> Uri -> Uri
withUserInfo = ffi "%2['clone']()['setUserInfo'](%1)"

withHost :: String -> Uri -> Uri
withHost = ffi "%2['clone']()['setHost'](%1)"

withPort :: String -> Uri -> Uri
withPort = ffi "%2['clone']()['setPort'](%1)"

withPath :: String -> Uri -> Uri
withPath = ffi "%2['clone']()['setPath'](%1)"

withQuery :: String -> Uri -> Uri
withQuery = ffi "%2['clone']()['setQuery'](%1)"

withAnchor :: String -> Uri -> Uri
withAnchor = ffi "%2['clone']()['setAnchor'](%1)"

-- | Removals

removeProtocol :: Uri -> Uri
removeProtocol = ffi "%1['clone']()['setProtocol'](null)"

removeUserInfo :: Uri -> Uri
removeUserInfo = ffi "%1['clone']()['setUserInfo'](null)"

removeHost :: Uri -> Uri
removeHost = ffi "%1['clone']()['setHost'](null)"

removePort :: Uri -> Uri
removePort = ffi "%1['clone']()['setPort'](null)"

removePath :: Uri -> Uri
removePath = ffi "%1['clone']()['setPath'](null)"

removeQuery :: Uri -> Uri
removeQuery = ffi "%1['clone']()['setQuery'](null)"

removeAnchor :: Uri -> Uri
removeAnchor = ffi "%1['clone']()['setAnchor'](null)"


-- | Other setters

addQueryParam :: String -> String -> Uri -> Uri
addQueryParam = ffi "%3['clone']()['addQueryParam'](%1,%2)"

replaceQueryParam :: String -> String -> Uri -> Uri
replaceQueryParam = ffi "%3['clone']()['replaceQueryParam'](%1,%2)"

-- The order of the arguments differ from the jsUri api, it is now
-- key -> oldValue -> newValue -> Uri -> Uri
replaceQueryParamValue :: String -> String -> String -> Uri -> Uri
replaceQueryParamValue = ffi "%4['clone']()['replaceQueryParam'](%1, %3, %2)"

deleteQueryParam :: String -> Uri -> Uri
deleteQueryParam = ffi "%2['clone']()['deleteQueryParam'](%1)"

deleteQueryParamValue :: String -> String -> Uri -> Uri
deleteQueryParamValue = ffi "%3['clone']()['deleteQueryParam'](%1,%2)"
