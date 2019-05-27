module UI.Component.HTML.Util where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Route (Route, routePath)


class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ = HP.class_ <<< HH.ClassName

href :: forall r i. Route -> HH.IProp ( href :: String | r) i
href = HP.href <<< routePath

voidHref :: forall r i. HH.IProp ( href :: String | r) i
voidHref = HP.href "javascript:undefined"

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""