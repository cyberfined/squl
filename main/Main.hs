{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.Squl
import Database.Squl.Builder
import Prelude hiding (all, any)

import qualified Data.ByteString.Lazy.Char8 as BS

test :: Select
test = select "users"
     & distinct
     . fields (allAgg "count" <| raw "login" "email" :| [])
     . inner join "posts" on "users.id = posts.author_id"
     . where_ ("users.name = 'john'" $| "users.name = 'ann'")
     . where_ (withQuery "users.id IN" $
         select "prefered_users AS pu" & fields (raw "pu.id" :| []))
     . order "users.id DESC"
     . order "users.login DESC"
     . group "users.gid"
     . limit 10
     . offset 20

main :: IO ()
main = BS.putStr queryStr
  where queryStr = buildQuery test <> "\n"
