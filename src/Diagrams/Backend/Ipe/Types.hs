{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Diagrams.Backend.Ipe.Types where

import Numeric

import Diagrams.TwoD.Types
import Diagrams.Core.Style
import Diagrams.Core.V


import Data.Default
import Data.Ratio
import Data.Semigroup
import Data.Text(Text)
import Data.Typeable

import qualified Data.Map  as M
import qualified Data.Text as T




--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Info

newtype Info = Info (Style ())

type instance V Info = ()

instance HasStyle Info where
    applyStyle s (Info s') = Info $ s <> s'

instance Default Info where
    def = Info mempty

--------------------------------------------------------------------------------
-- | Attributes for Info

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

toStyle   :: AttributeClass a => (Last t -> a) -> t -> Style v
toStyle f = attrToStyle . f . Last

applyAt     :: (HasStyle b, AttributeClass a) => (Last t -> a) -> t -> b -> b
applyAt f t = applyStyle (toStyle f t)



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
-- | IpeStyle



--------------------------------------------------------------------------------
-- | Preamble

type LaTeX    = Text
type Encoding = Text

data Preamble = Preamble (Maybe Encoding) LaTeX


--------------------------------------------------------------------------------
-- | Bitmap

newtype ColorKey = RGB Int -- in hex
                 deriving (Show,Read,Ord,Eq,Num)


data ColorSpace = DeviceGray | DeviceRGB (Maybe ColorKey) | DeviceCMYK
                deriving (Show,Eq)



data BMEncoding = Base64 | Hex
                deriving (Eq)


data Filter = FlateDecode | DCTDecode
            deriving (Show,Eq)

data FilterOrLength = Length Int
                    | Filter Filter
                      deriving (Eq)

type ImageData = Text



data Bitmap = Bitmap { identifier     :: Int
                     , width          :: Int
                     , height         :: Int
                     , colorspace     :: ColorSpace
                     , filterOrLength :: FilterOrLength
                     , bitmapEncoding :: Maybe BMEncoding
                     , imageData      :: ImageData
                     }

-- The contents of the <bitmap> element is the image data, either base64-encoded or in hexadecimal format. White space between bytes is ignored. If no filter is specified, pixels are stored row by row, with rows padded to a full byte boundary.




--------------------------------------------------------------------------------


-- | Represents the <page> tag
-- data Page a = Page [LayerDefinition] [ViewDefinition] [Object a]
--               deriving (Eq, Show)


type LayerDefinition = Text

-- | The definition of a view
-- make active layer into an index ?
data ViewDefinition = ViewDefinition { layerNames      :: [Text]
                                     , activeLayer     :: Text
                                     }
                      deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | An ipe-object. The main ``thing'' that defines the drawings

data IpeObject o where
    PathO         :: IsIpeNum a => Path a     -> IpeObject (Path a)
    UseO          :: IsIpeNum a => Use  a     -> IpeObject (Use a)
    TextO         ::               TextObject -> IpeObject TextObject
    ImageO        :: IsIpeNum a => Image a    -> IpeObject (Image a)
    GroupO        :: IsIpeNum a => Group a t  -> IpeObject (Group a t)

----------------------------------------
-- | Paths:
data Path a = Path Style' [Operation a]

type instance V (Path a) = ()

instance HasStyle (Path a) where
    applyStyle s (Path s' ops) = Path (s <> s') ops

instance Default (Path a) where
    def = Path mempty mempty

----------------------------------------
-- | A Use, basically a point
data Use a = Use Style' (P2 a)

type instance V (Use a) = ()

instance HasStyle (Use a) where
    applyStyle s (Use s' p) = Use (s <> s') p

-- instance Default (Use a) where
--     def = Use mempty origin


----------------------------------------
-- | A piece of text
data TextObject = TextObject Style' Text

----------------------------------------
-- | An image
data Image a = ImageRef   Style' (Rect a) Int
             | ImageEmbed Style' (Rect a) Bitmap


data Rect a = Rect a a a a

----------------------------------------
-- | Groups, collections of ipe objects with potentially transformations and or clipping paths
data Group a o = Group Style' (ClippingPath a) (IpeObjectList o)

type instance V (Group a o) = ()

instance HasStyle (Group a o) where
    applyStyle s (Group s' cp obs) = Group (s <> s') cp obs

instance Default (Group a NIL) where
    def = Group mempty mempty ONil



type ClippingPath a = [Operation a]

data NIL
data CONS a b

data IpeObjectList o where
    ONil  ::                                    IpeObjectList NIL
    OCons :: IpeObject o -> IpeObjectList os -> IpeObjectList (CONS o os)



-- oconcat :: IpeObjectList o -> IpeObjectList o' -> IpeObjectList (CONCAT o o')
-- EmptyObject   `oconcat` o = o
-- (PathO  p o') `oconcat` o = PathO  p (o' `oconcat` o)
-- (UseO   u o') `oconcat` o = UseO   u (o' `oconcat` o)
-- (TextO  t o') `oconcat` o = TextO  t (o' `oconcat` o)
-- (ImageO i o') `oconcat` o = ImageO i (o' `oconcat` o)
-- (GroupO g o') `oconcat` o = GroupO g (o' `oconcat` o)



--------------------------------------------------------------------------------
-- | Common Attributes








newtype Pin = Pin (Maybe PinType)
    deriving (Eq)

data PinType = Yes | Horizontal | Vertical
         deriving (Eq)

instance Show PinType where
    show Yes = "yes"
    show Horizontal = "h"
    show Vertical   = "v"


data TransformationType = Affine | Rigid | Translations
                        deriving (Eq)

instance Show TransformationType where
    show Affine       = "affine"
    show Rigid        = "rigid"
    show Translations = "translations"


type Matrix a = a

-- data CommonAttributes = CA { layer           :: Maybe LayerDefinition
--                            , matrix          :: Maybe Matrix
--                            , pin             :: Maybe Pin
--                            , transformations :: Maybe TransformationType
--                            }




--------------------------------------------------------------------------------
-- | Path Attributes


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


--               | Use        CommonAttributes
--                            SymbolName
--                            (P2 a)         -- pos
--                            (Maybe Color)  -- stroke
--                            (Maybe Color)  -- fill
--                            (Maybe SymVal) -- pen
--                            (Maybe SymSize) -- size







newtype MarkName = MarkName { markName' :: Last Mark }
                deriving (Show,Eq,Typeable,Semigroup)

instance Default MarkName where
    def = MarkName . Last $ def






data MarkOption = StrokeOption | FillOption | PenOption | SizeOption
                deriving (Show,Eq,Read,Typeable)

data Mark = Mark Text [MarkOption]
          deriving (Show,Eq,Typeable)

instance Default Mark where
    def = Mark "disk" [StrokeOption,SizeOption]




--------------------------------------------------------------------------------
-- | Text attributes


--------------------------------------------------------------------------------
-- | Image attributes

-- the image element is normally empty. However, it is allowed to omit the bitmap attribute. In this case, the <image> must carry all the attributes of the <bitmap> element, with the exception of id. The element contents is then the bitmap data, as described for <bitmap>.









type SymbolName = Text


type SymSize = Text
type SymVal  = Text

type Color = Text

data ArrowSize = ArrowSizeSym  SymVal
               | ArrowSizeReal Double
                 deriving (Show,Read,Eq)

data Arrow = Arrow { arrowName :: SymVal
                   , arrowSize :: ArrowSize
                   }

type Style' = Style ()


--------------------------------------------------------------------------------
-- | Operations defining a path


-- | type that represents a path in ipe.
data Operation a = MoveTo (P2 a)
                 | LineTo (P2 a)
                 | CurveTo (P2 a) (P2 a) (P2 a)
                 | QCurveTo (P2 a) (P2 a)
                 | Ellipse (Matrix a)
                 | ArcTo (Matrix a) (P2 a)
                 | Spline [P2 a]
                 | ClosedSpline [P2 a]
                 | ClosePath
                   deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | A type specifying the operations required on the numeric type used in ipe files

class (Fractional a, Ord a) => IsIpeNum a where
    showNum :: a -> Text
    fromSeq            :: Integer -> Maybe Integer -> a
    fromSeq x Nothing  = fromInteger x
    fromSeq x (Just y) = let x'        = fromInteger x
                             y'        = fromInteger y
                             asDecimal = head . dropWhile (>= 1) . iterate (* 0.1) in
                         signum x' * (abs x' + asDecimal y')

instance IsIpeNum Double where
    showNum = T.pack . show

instance IsIpeNum (Ratio Integer) where
    showNum r = T.pack . show $ (fromRat r :: Double)
    fromSeq x  Nothing = fromInteger x
    fromSeq x (Just y) = fst . head $ readSigned readFloat (show x ++ "." ++ show y)
