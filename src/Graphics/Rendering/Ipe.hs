{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.Ipe where

import Prelude hiding (writeFile)

import Control.Applicative((<$>))

-- import Diagrams.Prelude(Color)

import Diagrams.Core.Style(getAttr, AttributeClass, Style, addAttr )
import Diagrams.TwoD.Types

import Diagrams.Backend.Ipe.Types hiding (info, preamble)


import Diagrams.Attributes

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
-- | Rendering Ipe documents

-- | The doctype for ipe files
docType :: Doctype
docType = Doctype "ipe" $ Just (SystemID "ipe")


ipeDocument i = Document prologue (ipeElem i) []
    where
      prologue = Prologue [] (Just docType) []


ipeElem (IpeDocument mi mp ss bs ps) = Element "ipe" atts $ map NodeElement chs
    where
      atts = M.fromList [ ("version","70005")
                        , ("creator","diagrams-ipe")
                        ]
      chs  =    catMaybes [ fmap info     mi
                          , fmap preamble mp
                          ]
             <> map ipeStyle ss
             <> map bitmap   bs
             <> map page     ps



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

--------------------------------------------------------------------------------
-- | Preamble

preamble                :: Preamble -> Element
preamble (Preamble me l) = Element "preamble" atts [NodeContent l]
    where
      atts = maybe mempty (M.singleton "encoding") me


bitmap                                   :: Bitmap -> Element
bitmap (Bitmap i w h cs mFilt enc iData) = Element "bitmap" atts [NodeContent iData]
        where
          bitsPerComponent = 8
          fAtts            = maybe [] (\(Filter ft l) ->
                                          [ ("length",           showT l)
                                          , ("filter",           showT ft)
                                          ])
                             mFilt
          colKey           = (\(RGB k) ->   ("ColorKey",         showT k))
                             <$> colorKey cs
          encAtt           = if enc == Base64 then Just
                                            ("encoding" ,        "base64")
                             else Nothing
          atts             = M.fromList $ [ ("id",               showT i)
                                          , ("width",            showT w)
                                          , ("height",           showT h)
                                          , ("ColorSpace",       showCS cs)
                                          , ("BitsPerComponent", showT  bitsPerComponent)
                                          ] <> fAtts <> catMaybes [colKey, encAtt]
          showCS = T.pack . head . words . show


--------------------------------------------------------------------------------
-- | Styles


ipeStyle   :: IpeStyle -> Element
ipeStyle _ = Element "style" mempty mempty -- TODO


--------------------------------------------------------------------------------
-- | Pages


page                       :: Page -> Element
page (Page lds vds mn obs) = Element "page" atts chs
    where
      atts = mempty -- TODO
      chs' =    maybeToList (fmap notes mn)
             <> map layerDefinition lds
             <> map viewDefinition  vds
      chs  =    map NodeElement chs'
             <> ipeObjectList obs


notes   :: Notes -> Element
notes t =  Element "notes" mempty [NodeContent t]


layerDefinition       :: LayerDefinition -> Element
layerDefinition lName = Element "layer" atts []
    where
      atts = M.singleton "layer" lName
             -- TODO: optional attr: edit, whether or not the layer can be edited in ipe

viewDefinition                        :: ViewDefinition -> Element
viewDefinition (ViewDefinition lrs a) = Element "view" atts []
    where
      atts = M.fromList [ ("layers", T.unwords lrs)
                        , ("active", a)
                        ]
             -- TODO: Effect, marked



--------------------------------------------------------------------------------
-- | Ipe Objects


path              :: IsIpeNum a => Path a -> Element
path (Path s ops) = Element "path" atts [NodeContent $ opsText ops]
    where
      a    :: forall a. AttributeClass a => Maybe a
      a    = getAttr s
      atts = commonAttributes s <> attributes [ attr (a :: Maybe LineColor)
                                              , attr (a :: Maybe FillColor)
                                              , attr (a :: Maybe LineWidth)
                                              , attr (a :: Maybe DashingA)

                                              -- , attr (a :: Maybe LineCap)
                                              -- , attr (a :: Maybe LineJoin)

                                              -- , attr (a :: Maybe FillRule)

                                              -- , attr (a :: Maybe ArrowA)
                                              -- , attr (a :: Maybe RArrowA)
                                              , attr (a :: Maybe Opacity)
                                              -- , attr (a :: Maybe Tiling)
                                              -- , attr (a :: Maybe Gradient)

                                              ]

use            :: IsIpeNum a => Use a -> Element
use (Use s' p) = Element "use" atts []
    where
      -- if the name of the symbol has not been set yet, set it
      s    = addAttr (def :: MarkName) s'

      a    :: forall a. AttributeClass a => Maybe a
      a    = getAttr s
      atts =    commonAttributes s
             <> M.fromList [("pos",toStrictText p)]
             <> attributes [ attr (a :: Maybe MarkName)
                           , attr (a :: Maybe LineColor)
                           , attr (a :: Maybe FillColor)
                           , attr (a :: Maybe LineWidth)
                           , attr (a :: Maybe SymbolSize)
                           ]

text                 :: TextObject a -> Element
text (TextObject s l)= Element "text" atts [NodeContent l]
    where
      a    :: forall a. AttributeClass a => Maybe a
      a    = getAttr s
      atts = commonAttributes s <>
             attributes [ attr (a :: Maybe LineColor)
                        -- TODO: The other stuff
                        ]

image                     :: IsIpeNum a => Image a -> Element
image (ImageRef   s r i)  = let bmi = M.singleton "bitmap" (showT i) in
                            Element "image" (rect r <> bmi)            []
image (ImageEmbed s r bm) = Element "image" (rect r <> bitmapAttrs bm) [NodeContent . imageData $ bm]


group                  :: IsIpeNum a => Group a -> Element
group (Group s cp obs) = Element "group" atts (ipeObjectList obs)
    where
      -- a                :: forall a. AttributeClass a => Maybe a
      -- a                = getAttr s
      atts             = commonAttributes s <> clippingPathAttr
      clippingPathAttr = case cp of
                           [] -> mempty
                           _  -> M.singleton "clip" $ opsText cp


ipeObject            :: forall o. IpeObject o -> Element
ipeObject (PathO  p) = path  p
ipeObject (UseO   u) = use   u
ipeObject (TextO  t) = text  t
ipeObject (ImageO i) = image i
ipeObject (GroupO g) = group g


ipeObjectList              :: IpeObjectList -> [Node]
ipeObjectList ONil         = []
ipeObjectList (OCons o os) = (NodeElement . ipeObject $ o) : ipeObjectList os



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
-- | Info Attributes

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


--------------------------------------------------------------------------------
-- | Common Attributes (for IpeObjects)

-- | The attributes valid in *all* ipeObjects
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


----------------------------------------
-- | Attributes not valid in all, but several types of ipeObjects

instance ToAttribute LineColor where
    atName  = const "stroke"
    atVal   = colorVal . getLineColor

instance ToAttribute FillColor where
    atName  = const "fill"
    atVal   = colorVal . getFillColor

instance ToAttribute LineWidth where
    atName  = const "pen"
    atVal   = showT . getLineWidth


--------------------------------------------------------------------------------
-- | Path Attributes


instance ToAttribute Opacity where
    atName  = const "opacity"
    atVal   = opacityVal . getOpacity

instance ToAttribute DashingA where
    atName  = const "dash"
    atVal v = case getDashing v of
                Dashing _ _ -> "TODO"

-- -- dash
-- --     (optional) Either a symbolic name defined in a style sheet, or a dash pattern in PDF format, such as "[3 1] 0" for "three pixels on, one off, starting with the first pixel". If the attribute is missing, a solid line is drawn.



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




--------------------------------------------------------------------------------
-- | Use Attributes

instance ToAttribute MarkName where
    atName  = const "name"
    atVal v = case getLast . markName' $ v of
                Mark n ops -> toStrictT $ format "mark/{}({})" (n,map toT ops)
        where
          toT :: MarkOption -> Char
          toT = toLower . head . show


instance ToAttribute SymbolSize where
    atName  = const "size"
    atVal   = showT . getLast . symbolSize'


--------------------------------------------------------------------------------
-- | Text Attributes

-- type
--     (required) Possible values are label and minipage.
-- stroke
--     (optional) The stroke color. If the attribute is missing, black will be used.
-- size
--     (optional) The font size—either a symbolic name defined in a style sheet, or a real number. The default is "normal".
-- pos
--     (required) Two real numbers separated by white space, defining the position of the text on the paper.
-- width
--     (required for minipage objects, optional for label objects) The width of the object in points.
-- height
--     (optional) The total height of the object in points.
-- depth
--     (optional) The depth of the object in points.
-- valign
--     (optional) Possible values are top (default for a minipage object), bottom (default for a label object), center, and baseline.
-- halign
--     (optional, label only) Possible values are left, right, and center. left is the default. This determines the position of the reference point with respect to the text box.
-- style
--     (optional, minipage only) Selects a LaTeX "style" to be used for formatting the text, and must be a symbolic name defined in a style sheet. The standard style sheet defines the styles "normal", "center", "itemize", and "item". If the attribute is not present, the "normal" style is applied.
-- opacity
--     (optional) Opacity of the element. This must be a symbolic name. The default is 1.0, meaning fully opaque.


--------------------------------------------------------------------------------
-- | Image/Bitmap Attributes


bitmapAttrs = undefined


rect :: IsIpeNum a => Rect a -> M.Map Name Text
rect = M.singleton "rect" . toStrictText



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


--------------------------------------------------------------------------------
-- | Stuff with colors

colorVal :: forall c. Color c => c -> Text
colorVal = colorToRgbString


colorToRgbString :: forall c . Color c => c -> Text
colorToRgbString c = mconcat [ int r, " "
                             , int g, " "
                             , int b
                             ]
    where int d     = showT (round (d * 255) :: Int)
          (r,g,b,_) = colorToSRGBA c


opacityVal :: Double -> Text
opacityVal = const "1.0" -- TODO: this must be a symbolic name in Ipe


colorToOpacity :: forall c . Color c => c -> Double
colorToOpacity c = a
    where (_,_,_,a) = colorToSRGBA c
