{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Graphics.Rendering.Ipe where

import Prelude hiding (writeFile , FilePath)

import Control.Applicative((<$>))

-- import Diagrams.Prelude(Color)

import Diagrams.Attributes
import Diagrams.Core.Style(getAttr, AttributeClass, Style, addAttr )
import Diagrams.TwoD.Types

import Diagrams.Backend.Ipe.Attributes
import Diagrams.Backend.Ipe.Types hiding (info, preamble)

import Data.Maybe
import Data.Semigroup

import Filesystem.Path

import Graphics.Rendering.Ipe.Attributes
import Graphics.Rendering.Ipe.IpeWriteable

import Text.XML
import Text.Hamlet.XML

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L




--------------------------------------------------------------------------------
-- | Writing files

writeIpeDocument      :: FilePath -> IpeDocument -> IO ()
writeIpeDocument path = writeFile renderOps path . ipeDocument
    where
      renderOps = def


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
