{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Diagrams.Backend.Ipe where



import Control.Monad.State

import Data.Default
import Data.Typeable
import Data.Text(Text)

import Diagrams.Prelude

import Diagrams.TwoD.Types
import Diagrams.Backend.Ipe.Types hiding (Path)


import Graphics.Rendering.Ipe

import qualified Diagrams.Backend.Ipe.Types as I


import qualified Filesystem.Path as FP

--------------------------------------------------------------------------------

renderAsXML     :: (OrderedField a, Semigroup m, Monoid m, FromIpeObjects c) =>
                   IpeOptions -> QDiagram Ipe (V2 a) m -> c
renderAsXML ops = fromIpeObjects . unIR . renderDia Ipe (O ops)



writeAsXML          :: (OrderedField a, Semigroup m, Monoid m) =>
                       FP.FilePath -> IpeOptions -> QDiagram Ipe (V2 a) m -> IO ()
writeAsXML path ops = writeIpeDocument path . renderAsXML ops


--------------------------------------------------------------------------------

newtype IpeResult = IpeResult { unIR :: IpeObjectList }
    deriving (Typeable,Monoid)

data Ipe = Ipe
         deriving (Show,Eq,Typeable)


data IpeOptions = IpeOptions


instance Default IpeOptions where
    def = IpeOptions



instance Monoid (Render Ipe (V2 b)) where
  mempty = R mempty
  (R r1) `mappend` (R r2) = R $ r1 `mappend` r2


instance Default (Options Ipe (V2 b)) where
    def = O def

instance Num b => Backend Ipe (V2 b) where
    data Render  Ipe (V2 b) = R IpeResult
    type Result  Ipe (V2 b) = IpeResult
    data Options Ipe (V2 b) = O IpeOptions

    withStyle _ s t (R r) = R r -- AKA unimplemented. something like? R . IpeResult $ omap (applyStyle s) ol

    doRender _ opts (R r) = r









example :: Diagram Ipe R2
example = circle 1 # fc red # lw 0 ||| square 1 # fc green # lw 0

testPath :: FP.FilePath
testPath = "/tmp/out.ipe"



--------------------------------------------------------------------------------


instance (IsIpeNum a, OrderedField a) => Renderable (Segment Closed (V2 a)) Ipe where
    render b = render b . fromSegment
        where
          fromSegment :: (OrderedField a) => Segment Closed (V2 a) -> Path (V2 a)
          fromSegment = fromSegments . (:[])

instance (IsIpeNum a, OrderedField a) => Renderable (Trail (V2 a)) Ipe where
  render b = render b . pathFromTrail

instance (IsIpeNum a, OrderedField a) => Renderable (Path (V2 a)) Ipe where
  render _ path = R $ IpeResult . I.singleton . PathO $ I.Path mempty ops
      where
        ops = concatMap renderLocTrail . pathTrails $ path


-- instance Renderable Text Ipe where
--     render _ t = R $ IpeResult . I.singleton . TextO $ I.TextObject mempty t

--------------------------------------------------------------------------------

type PathState a    = (P2 a, [Operation a])
type PathRenderer a = State (PathState a)

getCurrentPos :: PathRenderer a (P2 a)
getCurrentPos = fst <$> get

getOperations :: PathRenderer a [Operation a]
getOperations = snd <$> get

setCurrentPos   :: P2 a -> PathRenderer a ()
setCurrentPos p = modify (\(_,ops) -> (p,ops))

goTo :: P2 a -> PathRenderer a ()
goTo = setCurrentPos

appendOp :: Operation a -> PathRenderer a ()
appendOp o = modify (\(p,ops) -> (p,o:ops))


-- | Given a vector, compute the point that we end up in if we follow the
-- vector starting at the current point
locatedAt   :: Num a => V2 a -> PathRenderer a (P2 a)
locatedAt v = translate v <$> getCurrentPos



renderSegment                              :: Num a => Segment Closed (V2 a) -> PathRenderer a ()
renderSegment (Linear      (OffsetClosed v)) = do
                                               q <- locatedAt v
                                               appendOp $ LineTo q
                                               goTo q
renderSegment (Cubic vp vq (OffsetClosed v)) = do
                                                 p <- locatedAt vp
                                                 q <- locatedAt vq
                                                 r <- locatedAt v
                                                 appendOp $ CurveTo p q r
                                                 goTo r

renderTrail   :: OrderedField a => Trail (V2 a) -> PathRenderer a ()
renderTrail t = do
                  mapM_ renderSegment . trailSegments $ t
                  when (isLoop t) $ appendOp ClosePath


renderLocTrail                    :: OrderedField a => Located (Trail (V2 a)) -> [Operation a]
renderLocTrail (viewLoc -> (p,t)) = reverse . evalState r $ (p,[])
    where
      r = appendOp (MoveTo p) >> renderTrail t >> getOperations


--------------------------------------------------------------------------------



-- $ do
--     -- Don't fill lines. diagrams-lib separates out lines and loops
--     -- for us, so if we see one line, they are all lines.
--     when (any (isLine . unLoc) . pathTrails $ p) $ setIgnoreFill True
--     return (R.renderPath p)

-- -- instance Renderable Text Ipe where
-- --   render _ = R . return . R.renderText
