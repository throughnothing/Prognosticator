module Data.Log where

import Prelude

import Data.Generic.Rep (class Generic)

data LogLevel = Dev | Prod
derive instance genericLogLevel :: Generic LogLevel _
derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel