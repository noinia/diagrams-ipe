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


singlePageDocument :: IsIpeNum a => IpeObjectList a -> IpeDocument
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


data Bitmap = Bitmap { identifier     :: Int
                     , width          :: Int
                     , height         :: Int
                     , colorspace     :: ColorSpace
                     , filter         :: Maybe Filter
                     , bitmapEncoding :: BMEncoding
                     , imageData      :: ImageData
                     }

-- The contents of the <bitmap> element is the image data, either base64-encoded or in hexadecimal format. White space between bytes is ignored. If no filter is specified, pixels are stored row by row, with rows padded to a full byte boundary.

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

--------------------------------------------------------------------------------


-- | Represents the <page> tag
data Page where
    Page :: IsIpeNum a =>
            [LayerDefinition] -> [ViewDefinition] -> Maybe Notes -> IpeObjectList a -> Page

instance Default Page where
    def = fromIpeObjects (mempty :: IpeObjectList Double)



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

data IpeObject a o where
    PathO         :: Path a       -> IpeObject a (Path a)
    UseO          :: Use  a       -> IpeObject a (Use a)
    TextO         :: TextObject a -> IpeObject a (TextObject a)
    ImageO        :: Image a      -> IpeObject a (Image a)
    GroupO        :: Group a      -> IpeObject a (Group a)

type instance V (IpeObject a o) = V2 a

instance HasStyle (IpeObject a o) where
    applyStyle s (PathO  p) = PathO  $ applyStyle s p
    applyStyle s (UseO   u) = UseO   $ applyStyle s u
    applyStyle s (TextO  t) = TextO  $ applyStyle s t
    applyStyle s (ImageO i) = ImageO $ applyStyle s i
    applyStyle s (GroupO g) = GroupO $ applyStyle s g


type StyleV2 a = Style (V2 a)


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

type instance V (TextObject a) = V2 a

instance HasStyle (TextObject a) where
    applyStyle s (TextObject s' t) = TextObject (s <> s') t

----------------------------------------
-- | An image
data Image a = ImageRef   (StyleV2 a) (Rect a) Int
             | ImageEmbed (StyleV2 a) (Rect a) Bitmap


data Rect a = Rect (P2 a) (P2 a)
            deriving (Show,Eq)


type instance V (Image a) = V2 a

instance HasStyle (Image a) where
    applyStyle s (ImageRef   s' r i) = ImageRef   (s <> s') r i
    applyStyle s (ImageEmbed s' r b) = ImageEmbed (s <> s') r b


----------------------------------------
-- | Groups, collections of ipe objects with potentially transformations and or clipping paths
data Group a = Group (StyleV2 a) (ClippingPath a) (IpeObjectList a)

type instance V (Group a) = V2 a

instance HasStyle (Group a) where
    applyStyle s (Group s' cp obs) = Group (s <> s') cp obs

instance Default (Group a) where
    def = Group mempty mempty mempty


type ClippingPath a = [Operation a]

--------------------------------------------------------------------------------
-- | Ipe Object lists


data IpeObjectList a where
    ONil  ::                                     IpeObjectList a
    OCons :: IpeObject a o -> IpeObjectList a -> IpeObjectList a

instance Monoid (IpeObjectList a) where
    mempty = ONil
    ONil          `mappend` ol = ol
    (OCons o ol') `mappend` ol = OCons o $ ol' `mappend` ol


singleton :: IpeObject a o -> IpeObjectList a
singleton = flip OCons ONil




omap                :: (forall o. IpeObject a o -> IpeObject a o)
                      -> IpeObjectList a -> IpeObjectList a
omap f ONil         = ONil
omap f (OCons o ol) = OCons (f o) $ omap f ol



--------------------------------------------------------------------------------
-- | Easy ways of constructing certain Ipe data types from IpeObjects

class FromIpeObjects c where
    fromIpeObjects :: IsIpeNum a => IpeObjectList a -> c

    fromIpeObject :: IsIpeNum a => IpeObject a o -> c
    fromIpeObject = fromIpeObjects . singleton

instance FromIpeObjects IpeDocument where
    fromIpeObjects obs = def { pages = [fromIpeObjects obs] }




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

-- type instance V (Operation a) = V2 a

-- instance Num a => Transformable (Operation a) where
--     transform t (MoveTo p)        = MoveTo $ transform t p
--     transform t (LineTo p)        = LineTo $ transform t p
--     transform t (CurveTo p q r)   = CurveTo  (transform t p) (transform t q) (transform t r)
--     transform t (QCurveTo p q)    = QCurveTo (transform t p) (transform t q)
--     transform t (ArcTo m p)       = error "transform: not implemented" -- ArcTo m  (transform t p) -- FIXME: Implement this!
--     transform t (Ellipse m)       = error "transform: not implemented" -- Ellipse m                -- FIXME Implement this
--     transform t (Spline ps)       = Spline       $ map (transform t) ps
--     transform t (ClosedSpline ps) = ClosedSpline $ map (transform t) ps
--     transform t ClosePath         = ClosePath


type Matrix a = a -- TODO

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
