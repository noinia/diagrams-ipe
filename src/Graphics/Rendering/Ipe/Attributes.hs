{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Rendering.Ipe.Attributes where

import Data.Char(toLower)
import Data.Maybe
import Data.Semigroup

import Data.Text(Text)
import Data.Text.Format

import Text.XML(Name)



import Diagrams.Attributes
import Diagrams.Backend.Ipe.Attributes
import Diagrams.Backend.Ipe.Types

import Diagrams.Core.Style(getAttr, AttributeClass, Style, addAttr )

import Graphics.Rendering.Ipe.IpeWriteable

import qualified Data.Map as M
import qualified Data.Text as T

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
-- | Stuff with colors

colorVal :: forall c. Color c => c -> Text
colorVal = colorToRgbString


colorToRgbString                             :: forall c . Color c => c -> Text
colorToRgbString (colorToSRGBA -> (r,g,b,_)) = T.intercalate " " . map showT $ [r,g,b]


opacityVal :: Double -> Text
opacityVal = const "1.0" -- TODO: this must be a symbolic name in Ipe


colorToOpacity :: forall c . Color c => c -> Double
colorToOpacity (colorToSRGBA -> (_,_,_,a)) = a
