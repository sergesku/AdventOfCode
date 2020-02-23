{-# LANGUAGE OverloadedStrings #-}

import Data.List (isPrefixOf)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.ByteString.Lazy.Char8 (ByteString, append, pack)

input :: ByteString
input = "yzbqklnj"

appendIntToBS :: ByteString -> Int -> ByteString
appendIntToBS bs = append bs . pack . show

checkMD5 :: String -> MD5Digest -> Bool
checkMD5 str = isPrefixOf str . show

solvWith :: ByteString -> String -> Int
solvWith bs str = head . filter (checkMD5 str . md5 . appendIntToBS bs) $ [1..]

main_part1 = print $ input `solvWith` "00000"
main_part2 = print $ input `solvWith` "000000"