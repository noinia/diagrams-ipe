{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Diagrams.Backend.Ipe.Render where

import Prelude hiding (writeFile)


import Diagrams.Core.Style(getAttr, AttributeClass, Style, addAttr )
import Diagrams.TwoD.Types
import Diagrams.Backend.Ipe.Types

import Data.Char(toLower)
import Data.Maybe
import Data.Semigroup

import Data.Text(Text)
import Data.Text.Format

import Text.XML
import Text.Hamlet.XML

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L


--------------------------------------------------------------------------------
-- | Generic functions and types to deal with attributes

type AttrMap = M.Map Name Text


-- | A class expressing how to construct a name and text value, that we use in the XML
-- representation.
--
-- Minimal implementation: either toAttr or both atName and atVal.
class ToAttribute v where
    atName :: v -> Name
    atName = fst . toAttr

    atVal  :: v -> Text
    atVal = snd . toAttr

    toAttr :: v -> (Name,Text)
    toAttr v = (atName v, atVal v)

-- | Existential data type wrapping around all things that can be convert to an attribute.
data Attribute = forall a. (ToAttribute a) => Attr a


-- | given a list of attributes, produce an attribute map that we use in the XML elements
attributes :: [Maybe Attribute] -> AttrMap
attributes = M.fromList . map f . catMaybes
             where
               f (Attr a') = toAttr a'

attr :: forall a. ToAttribute a => Maybe a -> Maybe Attribute
attr = fmap Attr

showT :: Show a => a -> Text
showT = T.pack . show

--------------------------------------------------------------------------------
-- | Rendering Ipe documents


-- | The doctype for ipe files
docType :: Doctype
docType = Doctype "ipe" $ Just (SystemID "ipe")



ipeDocument ipe = Document prologue ipe []
    where
      prologue = Prologue [] (Just docType) []





-- ipeElem mInfo mPreamble bitmaps styles pages = Element (M.fromList [ ("version","70005")
--                                                                    , ("creator","diagrams-ipe")
--                                                                    ])
--                                                        (map NodeContent chs)
--     where
--       chs = (catMaybes [mInfo,mPreamble]) <> bitmaps <> styles <> pages



info          :: Info -> Element
info (Info s) = Element "info" atts []
         where
           a    :: forall a. AttributeClass a => Maybe a
           a    = getAttr s
           atts = attributes [ attr (a :: Maybe Title)
                             , attr (a :: Maybe Author)
                             , attr (a :: Maybe Subject)
--                             , attr (a :: Maybe Keywords)
                             , attr (a :: Maybe PageMode)
                             , attr (a :: Maybe Created)
                             , attr (a :: Maybe Modified)
                             , attr (a :: Maybe NumberPages)
                             ]



instance ToAttribute Title where
    atName = const "title"
    atVal  = getLast . title'

instance ToAttribute Subject where
    atName = const "subject"
    atVal  = getLast . subject'

instance ToAttribute Author where
    atName = const "author"
    atVal  = getLast . author'

instance ToAttribute PageMode where
    atName = const "pagemode"
    atVal  = showT . getLast . pageMode'

instance ToAttribute NumberPages where
    atName  = const "numberpages"
    atVal v = case getLast . numberPages' $ v of
                True  -> "yes"
                False -> "no"

instance ToAttribute Created where
    atName = const "created"
    atVal  = showT . getLast . created'

instance ToAttribute Modified where
    atName = const "modified"
    atVal  = showT . getLast . modified'









-- preamble                :: Preamble -> Element
-- preamble (Preamble e l) = Element (attributes enc) [NodeContent l]
--     where
--       enc = maybeToList . fmap (\e -> ("encoding",e)) $ e


-- bitmap                                                              :: Bitmap -> Element
-- bitmap (Bitmap id width height cs filterOrLength mEncoding imgData) =
--     Element atts [NodeContent imgData]
--         where
--           bitsPerComponent = 8
--           colorKey         = undefined -- TODO: only if colorspace is DeviceRGB
--           atts = [] -- TODO



-- use :: IsIpeNum a => IpeObject a -> Element
-- use (Use common name pos mStroke mFill mPen mSize) = Element ats []
--     where
--       ats      = ("name",name) : ("pos", showP pos) : withCommon common <> specific
--       specific = attributes [ withN "stroke" mStroke
--                             , withN "fill"   mFill
--                             , withN "pen"    mPen
--                             , withN "size"   mSize
--                             ]
-- use _ = error |"works only for use"

-- showP = undefined -- TODO


-- path :: IseIpeNum a => IpeObject a -> Element
-- path (Path common mStroke mFill mDash mPen mCap mJoin mFRule mArr mRArr mOpacity mTiling mGradient ops) =
--     Element ats [NodeContent opsT]
--         where
--           ats      = withCommon common <> specific
--           specific = attributes [ withN "stroke"                mStroke
--                                 , withN "fill"                  mFill
--                                 , withN "dash"                  mDash
--                                 , withN "pen"                   mPen
--                                 , withN "cap"         . showT $ mCap
--                                 , withN "join"        . showT $ mJoin
--                                 , withN "fillrule"              mFRule
--                                 , withN "arrow"       . showT $ mArr
--                                 , withN "rarrow"      . showT $ mRArr
--                                 , withN "opacity"               mOpacity
--                                 , withN "tiling"                mTiling
--                                 , withN "gradient"              mGradient
--                                 ]



-- withCommon (CA mLayer mMatrix mPin mTrans) = attributes [ withN "layer"                     mLayer
--                                                         , withN "matrix"          . showT $ mMatrix
--                                                         , withN "pin"             . showT $ mPin
--                                                         , withN "transformations" . showT $ mTrans
--                                                         ]












-- -- stroke
-- --     (optional) The stroke color. If the attribute is missing, the shape will not be stroked.
-- -- fill
-- --     (optional) The fill color. If the attribute is missing, the shape will not be filled.
-- -- dash
-- --     (optional) Either a symbolic name defined in a style sheet, or a dash pattern in PDF format, such as "[3 1] 0" for "three pixels on, one off, starting with the first pixel". If the attribute is missing, a solid line is drawn.
-- -- pen
-- --     (optional) The line width, either symbolic (defined in a style sheet), or as a single real number. The default value is "normal".
-- -- cap
-- --     (optional) The line cap setting of PDF as an integer. If the argument is missing, the setting from the style sheet is used.
-- -- join
-- --     (optional) The line join setting of PDF as an integer. If the argument is missing, the setting from the style sheet is used.
-- -- fillrule
-- --     (optional) Possible values are wind and eofill, selecting one of two algorithms for determining whether a point lies inside a filled object. If the argument is missing, the setting from the style sheet is used.
-- -- arrow
-- --     (optional) The value consists of a symbolic name, say "triangle" for an arrow type (a symbol with name "arrow/triangle(spx)"), followed by a slash and the size of the arrow. The size is either a symbolic name (of type "arrowsize") defined in a style sheet, or a real number. If the attribute is missing, no arrow is drawn.
-- -- rarrow
-- --     (optional) Same for an arrow in the reverse direction (at the beginning of the first subpath).
-- -- opacity
-- --     (optional) Opacity of the element. This must be a symbolic name. The default is 1.0, meaning fully opaque.
-- -- tiling
-- --     (optional) A tiling pattern to be used to fill the element. The default is not to tile the element. If the element is not filled, then the tiling pattern is ignored.
-- -- gradient
-- --     (optional) A gradient pattern to be used to fill the element. If the element is not filled, then the gradient pattern is ignored. (The fill color is only used for rendering where gradients are not available, for instance currently in Postscript.) If gradient is set, then tiling is ignored.




path              :: IsIpeNum a => Path a -> Element
path (Path s ops) = Element "path" atts [NodeContent $ opsText ops]
    where
      a    :: forall a. AttributeClass a => Maybe a
      a    = getAttr s
      atts = commonAttributes s <> attributes [ -- TODO
                                              ]

use :: IsIpeNum a => Use a -> Element
use (Use s' p) = Element "use" atts []
    where
      -- if the name of the symbol has not been set yet, set it
      s    = addAttr (def :: MarkName) s'

      a    :: forall a. AttributeClass a => Maybe a
      a    = getAttr s
      atts =    commonAttributes s
             <> M.fromList [("pos",toStrictText p)]
             <> attributes [ -- TODO
                           ]

text :: TextObject -> Element
text = undefined


image :: IsIpeNum a => Image a -> Element
image = undefined


group                  :: IsIpeNum a => Group a o -> Element
group (Group s cp obs) = Element "group" atts (ipeObjectList obs)
    where
      -- a                :: forall a. AttributeClass a => Maybe a
      -- a                = getAttr s
      atts             = commonAttributes s <> clippingPathAttr
      clippingPathAttr = case cp of
                           [] -> mempty
                           _  -> M.fromList [("clip", opsText cp)]


ipeObject            :: forall o. IpeObject o -> Element
ipeObject (PathO  p) = path  p
ipeObject (UseO   u) = use   u
ipeObject (TextO  t) = text  t
ipeObject (ImageO i) = image i
ipeObject (GroupO g) = group g


ipeObjectList              :: forall o. IpeObjectList o -> [Node]
ipeObjectList ONil         = []
ipeObjectList (OCons o os) = (NodeElement . ipeObject $ o) : ipeObjectList os




--------------------------------------------------------------------------------

commonAttributes   :: Style v -> AttrMap
commonAttributes s = attributes [ attr (a :: Maybe Layer)
                                , attr (a :: Maybe Pin)
                                , attr (a :: Maybe Transformations)
                                ]
    where
      a :: forall a. AttributeClass a => Maybe a
      a = getAttr s




instance ToAttribute Layer where
    atName = const "layer"
    atVal  = getLast . layer'

instance ToAttribute Pin where
    atName  = const "pin"
    atVal v = case getLast . pin' $ v of
                Pinned     -> "yes"
                Horizontal -> "h"
                Vertical   -> "v"


instance ToAttribute Transformations  where
    atName = const "transformations"
    atVal  = T.toLower . showT . getLast . transformations'

--------------------------------------------------------------------------------

instance ToAttribute MarkName where
    atName  = const "name"
    atVal v = case getLast . markName' $ v of
                Mark n ops -> toStrictT $ format "mark/{}({})" (n,map toT ops)
        where
          toT :: MarkOption -> Char
          toT = toLower . head . show

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
