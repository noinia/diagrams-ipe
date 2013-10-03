{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Diagrams.Backend.Ipe.Attributes where



import Data.Default
import Data.Semigroup
import Data.Text
import Data.Typeable

import Diagrams.Backend.Ipe.Types

import Diagrams.Core.Style


import Diagrams.Attributes


--------------------------------------------------------------------------------
-- | Generic stuff

toStyle   :: AttributeClass a => (Last t -> a) -> t -> Style v
toStyle f = attrToStyle . f . Last

applyAt     :: (HasStyle b, AttributeClass a) => (Last t -> a) -> t -> b -> b
applyAt f t = applyStyle (toStyle f t)

--------------------------------------------------------------------------------
-- | Info

instance AttributeClass Title
instance AttributeClass Subject
instance AttributeClass Author
-- instance AttributeClass Keyword
instance AttributeClass NumberPages
instance AttributeClass Created
instance AttributeClass Modified
instance AttributeClass PageMode


newtype Title = Title { title' :: Last Text }
              deriving (Show,Eq,Ord,Semigroup,Typeable)

newtype Subject = Subject { subject' :: Last Text }
                deriving (Show,Eq,Ord,Semigroup,Typeable)

newtype Author = Author { author' :: Last Text }
               deriving (Show,Eq,Ord,Semigroup,Typeable)

newtype Keyword = Keyword { keyword' :: Last Text }
                deriving (Show,Eq,Ord,Typeable)

-- TODO: instance semigroup that adds this to a list or so

newtype NumberPages = NumberPages { numberPages' :: Last Bool }
                    deriving (Eq,Typeable,Semigroup)

newtype Created = Created { created' :: Last DateTime }
    deriving (Show,Eq,Ord,Semigroup,Typeable)

newtype Modified = Modified { modified' :: Last DateTime }
    deriving (Show,Eq,Ord,Semigroup,Typeable)


newtype PageMode = PageMode { pageMode' :: Last FullScreen }
                 deriving (Show,Eq,Typeable,Semigroup)



data FullScreen = FullScreen
                deriving (Eq,Typeable)

instance Show FullScreen where
    show FullScreen = "fullscreen"


newtype DateTime = DateTime (Last Text)
    deriving (Eq,Ord,Typeable,Semigroup)

instance Show DateTime where
    show (DateTime (Last t)) = "D:" <> show t
--    creation time in PDF format, e.g. "D:20030127204100".


title :: HasStyle a => Text -> a -> a
title = applyAt Title

subject :: HasStyle a => Text -> a -> a
subject = applyAt Subject

author :: HasStyle a => Text -> a -> a
author = applyAt Author

numberPages :: HasStyle a => Bool -> a -> a
numberPages = applyAt NumberPages

pagemode :: HasStyle a => FullScreen -> a -> a
pagemode = applyAt PageMode

created :: HasStyle a => DateTime -> a -> a
created = applyAt Created

modified :: HasStyle a => DateTime -> a -> a
modified = applyAt Modified

--------------------------------------------------------------------------------

instance AttributeClass Layer
---- instance AttributeClass Matrix
instance AttributeClass Pin
instance AttributeClass Transformations



----------------------------------------

newtype Layer = Layer { layer' :: Last LayerDefinition }
              deriving (Show,Eq,Ord,Semigroup,Typeable)

-- newtype Matrix = Layer { ' :: Last Matrx3 a }
--               deriving (Show,Eq,Ord,Semigroup,Typeable)

newtype Pin = Pin {pin' :: Last PinType}
    deriving (Eq,Typeable,Semigroup)

newtype Transformations = Transformations {transformations' :: Last TransformationType}
    deriving (Eq,Typeable,Semigroup)






data PinType = Pinned | Horizontal | Vertical
         deriving (Show,Eq,Typeable)



data TransformationType = Affine | Rigid | Translations
                        deriving (Show,Eq,Typeable)





layer :: HasStyle a => LayerDefinition -> a -> a
layer = applyAt Layer

pin :: HasStyle a => PinType -> a -> a
pin = applyAt Pin

transformations :: HasStyle a => TransformationType -> a -> a
transformations = applyAt Transformations






--------------------------------------------------------------------------------
-- | Path Attributes


type SymSize = Text
type SymVal  = Text


data ArrowSize = ArrowSizeSym  SymVal
               | ArrowSizeReal Double
                 deriving (Show,Read,Eq)

data Arrow = Arrow { arrowName :: SymVal
                   , arrowSize :: ArrowSize
                   }



--                            (Maybe Color)  -- stroke
--                            (Maybe Color)  -- fill
--                            (Maybe SymVal) -- dash
--                            (Maybe SymVal) -- pen
--                            (Maybe Int) -- line cap
--                            (Maybe Int) -- line join
--                            (Maybe SymVal) -- fillrule
--                            (Maybe Arrow) -- forward arrow
--                            (Maybe Arrow) -- backward arrow
--                            (Maybe SymVal) -- opaciity
--                            (Maybe SymVal) -- tiling
--                            (Maybe SymVal) -- tiling
--                            (Maybe SymVal) -- gradient


--------------------------------------------------------------------------------
-- | Use Attributes


instance AttributeClass MarkName
-- Line (Stroke) Color
-- Fill Color
-- LineWidth (Pen)
instance AttributeClass SymbolSize



newtype MarkName = MarkName { markName' :: Last Mark }
                deriving (Show,Eq,Typeable,Semigroup)

instance Default MarkName where
    def = MarkName . Last $ def


newtype SymbolSize = SymbolSize { symbolSize' :: Last SymVal }
                   deriving (Show,Eq,Typeable,Semigroup)


instance Default SymbolSize where
    def = SymbolSize . Last $ "normal"



data MarkOption = StrokeOption | FillOption | PenOption | SizeOption
                deriving (Show,Eq,Read,Typeable)

data Mark = Mark Text [MarkOption]
          deriving (Show,Eq,Typeable)

instance Default Mark where
    def = Mark "disk" [StrokeOption,SizeOption]


type SymbolName = Text


--------------------------------------------------------------------------------
-- | Text attributes


--------------------------------------------------------------------------------
-- | Image attributes

-- the image element is normally empty. However, it is allowed to omit the bitmap attribute. In this case, the <image> must carry all the attributes of the <bitmap> element, with the exception of id. The element contents is then the bitmap data, as described for <bitmap>.
