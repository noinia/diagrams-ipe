{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Diagrams.Backend.Ipe.Types where

import Numeric


import Diagrams.TwoD.Types
import Diagrams.Core.Style
import Diagrams.Core.V
import Diagrams.Transform


import Data.Default
import Data.Ratio
import Data.Semigroup
import Data.Text(Text)
import Data.Typeable

import qualified Data.Map  as M
import qualified Data.Text as T



--------------------------------------------------------------------------------
-- | The Ipe Element

data IpeDocument = IpeDocument { info     :: Maybe Info
                               , preamble :: Maybe Preamble
                               , styles   :: [IpeStyle]
                               , bitmaps  :: [Bitmap]
                               , pages    :: [Page]
                               }

instance Default IpeDocument where
    def = IpeDocument Nothing Nothing [] [] [def]


singlePageDocument :: IpeObjectList -> IpeDocument
singlePageDocument = fromIpeObjects


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

data IpeStyle = IpeStyle
              deriving (Show,Eq)


-- TODO: Make this a more useful data type


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


colorKey                :: ColorSpace -> Maybe ColorKey
colorKey (DeviceRGB mc) = mc
colorKey _              = Nothing


data BMEncoding = Base64 | Hex
                deriving (Eq,Show)


instance Default BMEncoding where
    def = Hex


data FilterType = FlateDecode | DCTDecode
                  deriving (Show,Eq)

-- | The filter type and the length of the image data
data Filter = Filter FilterType Int
              deriving (Eq)

type ImageData = Text



data Bitmap = Bitmap { identifier     :: Int
                     , width          :: Int
                     , height         :: Int
                     , colorspace     :: ColorSpace
                     , filter         :: Maybe Filter
                     , bitmapEncoding :: BMEncoding
                     , imageData      :: ImageData
                     }

-- The contents of the <bitmap> element is the image data, either base64-encoded or in hexadecimal format. White space between bytes is ignored. If no filter is specified, pixels are stored row by row, with rows padded to a full byte boundary.




--------------------------------------------------------------------------------


-- | Represents the <page> tag
data Page = Page [LayerDefinition] [ViewDefinition] (Maybe Notes) IpeObjectList


instance Default Page where
    def = fromIpeObjects mempty



instance FromIpeObjects Page where
    fromIpeObjects = Page [] [] Nothing




type Notes = Text

-- TODO: pages may have titles and subtitles



type LayerName       = Text
type LayerDefinition = LayerName

-- | The definition of a view
-- make active layer into an index ?
data ViewDefinition = ViewDefinition { layerNames      :: [LayerName]
                                     , activeLayer     :: LayerName
                                     }
                      deriving (Eq, Show)

instance Default ViewDefinition where
    def = ViewDefinition ["alpha"] "alpha"


--------------------------------------------------------------------------------
-- | An ipe-object. The main ``thing'' that defines the drawings

data IpeObject o where
    PathO         :: IsIpeNum a => Path a       -> IpeObject (Path a)
    UseO          :: IsIpeNum a => Use  a       -> IpeObject (Use a)
    TextO         :: IsIpeNum a => TextObject a -> IpeObject (TextObject a)
    ImageO        :: IsIpeNum a => Image a      -> IpeObject (Image a)
    GroupO        :: IsIpeNum a => Group a      -> IpeObject (Group a)

type instance V (IpeObject (Path a)) = V2 a
type instance V (IpeObject (Use a)) = V2 a
type instance V (IpeObject (TextObject a)) = V2 a
type instance V (IpeObject (Image a)) = V2 a
type instance V (IpeObject (Group a)) = V2 a


----------------------------------------
-- | Paths:
data Path a = Path (StyleV2 a) [Operation a]

type instance V (Path a) = V2 a

instance HasStyle (Path a) where
    applyStyle s (Path s' ops) = Path (s <> s') ops

instance Default (Path a) where
    def = mempty

instance Monoid (Path a) where
    mempty = Path mempty mempty
    (Path s os) `mappend` (Path s' os') = Path (s `mappend` s') (os `mappend` os')

----------------------------------------
-- | A Use, basically a point
data Use a = Use (StyleV2 a) (P2 a)

type instance V (Use a) = V2 a

instance HasStyle (Use a) where
    applyStyle s (Use s' p) = Use (s <> s') p

-- instance Default (Use a) where
--     def = Use mempty origin


----------------------------------------
-- | A piece of text
data TextObject a = TextObject (StyleV2 a) LaTeX

----------------------------------------
-- | An image
data Image a = ImageRef   (StyleV2 a) (Rect a) Int
             | ImageEmbed (StyleV2 a) (Rect a) Bitmap


data Rect a = Rect (P2 a) (P2 a)
            deriving (Show,Eq)

----------------------------------------
-- | Groups, collections of ipe objects with potentially transformations and or clipping paths
data Group a = Group (StyleV2 a) (ClippingPath a) IpeObjectList

type instance V (Group a) = V2 a

instance HasStyle (Group a) where
    applyStyle s (Group s' cp obs) = Group (s <> s') cp obs

instance Default (Group a) where
    def = Group mempty mempty mempty


type ClippingPath a = [Operation a]



data IpeObjectList where
    ONil  ::                                 IpeObjectList
    OCons :: IpeObject o -> IpeObjectList -> IpeObjectList

instance Monoid IpeObjectList where
    mempty = ONil
    ONil          `mappend` ol = ol
    (OCons o ol') `mappend` ol = OCons o $ ol' `mappend` ol


singleton :: IpeObject o -> IpeObjectList
singleton = flip OCons ONil

omap :: (forall a. IpeObject a -> IpeObject a) -> IpeObjectList -> IpeObjectList
omap f ONil         = ONil
omap f (OCons o ol) = OCons (f o) $ omap f ol



--------------------------------------------------------------------------------
-- | Easy ways of constructing certain Ipe data types from IpeObjects

class FromIpeObjects c where
    fromIpeObjects :: IpeObjectList -> c

    fromIpeObject :: IpeObject o -> c
    fromIpeObject = fromIpeObjects . singleton

instance FromIpeObjects IpeDocument where
    fromIpeObjects obs = def { pages = [fromIpeObjects obs] }












--------------------------------------------------------------------------------
-- | Common Attributes


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


type Matrix a = a



layer :: HasStyle a => LayerDefinition -> a -> a
layer = applyAt Layer

pin :: HasStyle a => PinType -> a -> a
pin = applyAt Pin

transformations :: HasStyle a => TransformationType -> a -> a
transformations = applyAt Transformations






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




--------------------------------------------------------------------------------
-- | Text attributes


--------------------------------------------------------------------------------
-- | Image attributes

-- the image element is normally empty. However, it is allowed to omit the bitmap attribute. In this case, the <image> must carry all the attributes of the <bitmap> element, with the exception of id. The element contents is then the bitmap data, as described for <bitmap>.









type SymbolName = Text


type SymSize = Text
type SymVal  = Text


data ArrowSize = ArrowSizeSym  SymVal
               | ArrowSizeReal Double
                 deriving (Show,Read,Eq)

data Arrow = Arrow { arrowName :: SymVal
                   , arrowSize :: ArrowSize
                   }

type Style' = Style ()

type StyleV2 a = Style (V2 a)

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

type instance V (Operation a) = V2 a

instance Num a => Transformable (Operation a) where
    transform t (MoveTo p)        = MoveTo $ transform t p
    transform t (LineTo p)        = LineTo $ transform t p
    transform t (CurveTo p q r)   = CurveTo  (transform t p) (transform t q) (transform t r)
    transform t (QCurveTo p q)    = QCurveTo (transform t p) (transform t q)
    transform t (ArcTo m p)       = error "transform: not implemented" -- ArcTo m  (transform t p) -- FIXME: Implement this!
    transform t (Ellipse m)       = error "transform: not implemented" -- Ellipse m                -- FIXME Implement this
    transform t (Spline ps)       = Spline       $ map (transform t) ps
    transform t (ClosedSpline ps) = ClosedSpline $ map (transform t) ps
    transform t ClosePath         = ClosePath



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
