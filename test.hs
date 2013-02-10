module Main where

import FFI
import Uri
import Prelude

main :: Fay ()
main = do
  let uri = newUri "http://user:pass@www.example.com:80/ppp?a=b#c"
  putStrLn . toString . newUri =<< currentUri
  print uri
  putStrLn "--"
  mapM_ (putStrLn . ($ uri)) $
    toString : map (fromNullable .) [protocol, host, port, path, query, anchor]
  putStrLn "--"
  mapM_ (putStrLn . toString . ($ uri))
    [withProtocol "https", withUserInfo "foo:bar", withHost "example.net", withPort "90", withPath "path", withQuery "e=f", withAnchor "g"]
  putStrLn "--"
  mapM_ (putStrLn . toString . ($ uri))
    [removeProtocol, removeUserInfo, removeHost, removePort, removePath, removeQuery, removeAnchor]
  putStrLn "--"
  mapM_ (putStrLn . toString . ($ uri))
    [addQueryParam "c" "d", replaceQueryParam "a" "e", replaceQueryParamValue "a" "b" "f"
    ,deleteQueryParam "a", deleteQueryParamValue "a" "b"]
  putStrLn "--"
  mapM_ (putStrLn . ($ uri)) $ map (fromNullable .)
    [protocol . removeProtocol
    ,userInfo . removeUserInfo
    ,host     . removeHost
    ,port     . removePort
    ,path     . removePath
    ,query    . removeQuery
    ,anchor   . removeAnchor]

fromNullable :: Nullable String -> String
fromNullable Null = "null"
fromNullable (Nullable s) = "'" ++ s ++ "'"
