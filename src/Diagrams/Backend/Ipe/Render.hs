{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Diagrams.Backend.Ipe.Render where

import Prelude hiding (writeFile)


import Diagrams.Backend.Ipe.Types

import Data.Maybe
import Data.Monoid



import Text.XML
import Text.Hamlet.XML

import qualified Data.Map as M
import qualified Data.Text as T

-- type IpeXML = Element


attributes = M.fromList . catMaybes


docType = Doctype "ipe" $ Just (SystemID "ipe")



ipeDocument ipe = Document prologue ipe []
    where
      prologue = Prologue [] docType []



ipeElem mInfo mPreamble bitmaps styles pages = Element (M.fromList [ ("version","70005")
                                                                   , ("creator","diagrams-ipe")
                                                                   ])
                                                       (map NodeContent chs)
    where
      chs = (catMaybes [mInfo,mPreamble]) <> bitmaps <> styles <> pages


info :: Info -> Element
info Info (mTitle mAuthor mSubject mKeywords mPageMode mCreated mModified mNumberPages) =
    Element atts []
        where
          atts = attributes $ [ withN "title"                 mTitle
                              , withN "author"                mAuthor
                              , withN "subject"               mSubject
                              , withN "keywords"              mKeywords
                              , withN "pagemode"    . showT $ mPageMode
                              , withN "created"     . showT $ mCreated
                              , withN "modified"    . showT $ mModified
                              , withN "numberpages" . showT $ mNumberPages
                              ]

showT = T.pack . show

withN n  = fmap (\t -> (n,t))

preamble                :: Preamble -> Element
preamble (Preamble e l) = Element (attributes enc) [NodeContent l]
    where
      enc = maybeToList . fmap (\e -> ("encoding",e)) $ e


bitmap                                                              :: Bitmap -> Element
bitmap (Bitmap id width height cs filterOrLength mEncoding imgData) =
    Element atts [NodeContent imgData]
        where
          bitsPerComponent = 8
          colorKey         = undefined -- TODO: only if colorspace is DeviceRGB
          atts = [] -- TODO



use :: IsIpeNum a => IpeObject a -> Element
use (Use common name pos mStroke mFill mPen mSize) = Element ats []
    where
      ats      = ("name",name) : ("pos", showP pos) : withCommon common <> specific
      specific = attributes [ withN "stroke" mStroke
                            , withN "fill"   mFill
                            , withN "pen"    mPen
                            , withN "size"   mSize
                            ]
use _ = error |"works only for use"

showP = undefined -- TODO


path :: IseIpeNum a => IpeObject a -> Element
path (Path common mStroke mFill mDash mPen mCap mJoin mFRule mArr mRArr mOpacity mTiling mGradient ops) =
    Element ats [NodeContent opsT]
        where
          ats      = withCommon common <> specific
          specific = attributes [ withN "stroke"                mStroke
                                , withN "fill"                  mFill
                                , withN "dash"                  mDash
                                , withN "pen"                   mPen
                                , withN "cap"         . showT $ mCap
                                , withN "join"        . showT $ mJoin
                                , withN "fillrule"              mFRule
                                , withN "arrow"       . showT $ mArr
                                , withN "rarrow"      . showT $ mRArr
                                , withN "opacity"               mOpacity
                                , withN "tiling"                mTiling
                                , withN "gradient"              mGradient
                                ]



withCommon (CA mLayer mMatrix mPin mTrans) = attributes [ withN "layer"                     mLayer
                                                        , withN "matrix"          . showT $ mMatrix
                                                        , withN "pin"             . showT $ mPin
                                                        , withN "transformations" . showT $ mTrans
                                                        ]




-- stroke
--     (optional) The stroke color. If the attribute is missing, the shape will not be stroked.
-- fill
--     (optional) The fill color. If the attribute is missing, the shape will not be filled.
-- dash
--     (optional) Either a symbolic name defined in a style sheet, or a dash pattern in PDF format, such as "[3 1] 0" for "three pixels on, one off, starting with the first pixel". If the attribute is missing, a solid line is drawn.
-- pen
--     (optional) The line width, either symbolic (defined in a style sheet), or as a single real number. The default value is "normal".
-- cap
--     (optional) The line cap setting of PDF as an integer. If the argument is missing, the setting from the style sheet is used.
-- join
--     (optional) The line join setting of PDF as an integer. If the argument is missing, the setting from the style sheet is used.
-- fillrule
--     (optional) Possible values are wind and eofill, selecting one of two algorithms for determining whether a point lies inside a filled object. If the argument is missing, the setting from the style sheet is used.
-- arrow
--     (optional) The value consists of a symbolic name, say "triangle" for an arrow type (a symbol with name "arrow/triangle(spx)"), followed by a slash and the size of the arrow. The size is either a symbolic name (of type "arrowsize") defined in a style sheet, or a real number. If the attribute is missing, no arrow is drawn.
-- rarrow
--     (optional) Same for an arrow in the reverse direction (at the beginning of the first subpath).
-- opacity
--     (optional) Opacity of the element. This must be a symbolic name. The default is 1.0, meaning fully opaque.
-- tiling
--     (optional) A tiling pattern to be used to fill the element. The default is not to tile the element. If the element is not filled, then the tiling pattern is ignored.
-- gradient
--     (optional) A gradient pattern to be used to fill the element. If the element is not filled, then the gradient pattern is ignored. (The fill color is only used for rendering where gradients are not available, for instance currently in Postscript.) If gradient is set, then tiling is ignored.








--page objects
