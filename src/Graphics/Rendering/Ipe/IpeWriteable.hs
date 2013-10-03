{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.Ipe.IpeWriteable where

import Data.Monoid
import Data.Text(Text)
import Data.Text.Format

import Diagrams.TwoD.Types

import Diagrams.Backend.Ipe.Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

--------------------------------------------------------------------------------


-- | Minimal implementation: toText
class IsIpeNum (IpeNum c) => IsIpeWriteable c where
    type IpeNum c

    toOnly :: c -> Only L.Text
    toOnly = Only . toText
    toText :: c -> L.Text

    toStrictText :: c -> T.Text
    toStrictText = toStrictT . toText

    -- toString  :: Coordinate a => c a -> String
    -- toString = T.unpack . toText

    textSepBy        :: [c] -> L.Text -> L.Text
    xs `textSepBy` s =  L.intercalate s . map toText $ xs




-- -- instance IsIpeWriteable Matrix3 where
-- --     -- | Note, we only use the first 6 values (the first two rows)
-- --     --   furthermore, the order of the ipe matrices is switched again.
-- --     toText m = let [[a,b,c],[d,e,f],_] = matrix3ToLists m
-- --                    ipeM                = [a, c, e, b, d, f] in
-- --                T.intercalate " " . map (T.pack . toIpeOut) $ ipeM

instance IsIpeNum a => IsIpeWriteable (P2 a) where
    type IpeNum (P2 a) = a
    toText (unp2 -> (x,y)) = format "{} {}" $ map showNum [x,y]

instance IsIpeNum a => IsIpeWriteable (Rect a) where
    type IpeNum (Rect a) = a
    toText (Rect p1 p2) = format "{} {}" $ map toText [p1,p2]

instance IsIpeNum a => IsIpeWriteable (Operation a) where
    type IpeNum (Operation a) = a

    toText (MoveTo p)         = format "{} m"       $ toOnly p
    toText (LineTo p)         = format "{} l"       $ toOnly p
    toText (CurveTo p q r)    = format "{} {} {} c" $ map toText [p,q,r]
    toText (QCurveTo p q)     = format "{} {} q"    $ map toText [p,q]
    -- toText (Ellipse m)        = format "{} e"       $ toOnly m
    -- toText (ArcTo m p)        = format "{} {} a"      [toText m , toText p]
    toText (Spline pts)       = format "{} s"       $ Only $ pts `textSepBy` " "
    toText (ClosedSpline pts) = format "{} u"       $ Only $ pts `textSepBy` " "
    toText ClosePath          = "h"


opsText     :: IsIpeNum a => [Operation a] -> Text
opsText ops = toStrictT $ (ops `textSepBy` "\n") `L.append` "\n"


toStrictT :: L.Text -> T.Text
toStrictT = mconcat . L.toChunks
