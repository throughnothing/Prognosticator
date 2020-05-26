module Capability.Express where

import Prelude

import Data.Maybe (Maybe)
import Express.Foreign as EF
import Foreign (Foreign)
import Node.Express.Handler as EH
import Node.Express.Request as EReq
import Node.Express.Response as ERes

class Monad m <= ExpressReq m where
  getField :: String -> m (Maybe Foreign)
  getBody' :: m Foreign
  getPath :: m String

class Monad m <= ExpressRes m where
  send :: ∀ a. a -> m Unit
  sendFile :: String -> m Unit
  redirect :: String -> m Unit
  setStatus :: Int -> m Unit

class Monad m <= ExpressH m where
  next :: m Unit

instance handlerMExpressReq :: ExpressReq (EH.HandlerM) where
  getField = EF.getField
  getBody' = EReq.getBody'
  getPath = EReq.getPath

instance handlerMExpressRes :: ExpressRes (EH.HandlerM) where
  send = ERes.send
  sendFile = ERes.sendFile
  redirect = ERes.redirect
  setStatus = ERes.setStatus

instance handlerMExpressH :: ExpressH (EH.HandlerM) where
  next = EH.next
