module Data.Util where

import Data.Maybe (Maybe(..))

nonEmptyStr :: String -> Maybe String
nonEmptyStr "" = Nothing
nonEmptyStr s = Just s