module UI (launchUI) where

import Brick
import Brick.Main (App(..), halt, defaultMain, neverShowCursor)
import Brick.Types (Widget, BrickEvent(..), EventM)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.AttrMap (attrName)
import qualified Graphics.Vty as V
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

type Name = ()

data St = St [[FilePath]] Int

drawUI :: St -> [Widget Name]
drawUI (St groups selected) =
    [center $ borderWithLabel (str "Duplicate Files") $
        vBox $ zipWith drawGroup [0..] groups]
  where
    drawGroup i group =
        let header = if i == selected then withAttr (attrName "selected") (str "→ Group") else str "  Group"
         in vBox (header : map (str . ("  - " ++)) group)

app :: App St e Name
app = App
  { appDraw = drawUI
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const (attrMap V.defAttr [(attrName "selected", V.white `on` V.blue)])
  , appChooseCursor = neverShowCursor
  }

handleEvent :: BrickEvent Name e -> EventM Name St ()
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    St gs sel <- get
    put $ St gs (min (length gs - 1) (sel + 1))
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    St gs sel <- get
    put $ St gs (max 0 (sel - 1))
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

launchUI :: [[FilePath]] -> IO ()
launchUI groups = void $ defaultMain app (St groups 0)
