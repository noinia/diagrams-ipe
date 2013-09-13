module Diagrams.Backend.Ipe.Types where

import Numeric

import Diagrams.TwoD.Types

import Data.Ratio
import Data.Monoid
import Data.Text(Text)

import qualified Data.Map  as M
import qualified Data.Text as T
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Info

data PageMode = FullScreen
              deriving (Eq)

instance Show PageMode where
    show FullScreen = "fullscreen"

newtype NumberPages = NumberPages Bool
                    deriving (Eq)

instance Show PageMode where
    show (NumberPages False) = "no"
    show (NumberPages True)  = "yes"

newtype DateTime = DateTime Text

instance Show DateTime where
    show dt = "D:" <> show dt
--    creation time in PDF format, e.g. "D:20030127204100".


data Info = Info { title       :: Maybe Text        -- ^ title
                 , author      :: Maybe Text        -- ^ author
                 , subject     :: Maybe Text        -- ^ subject
                 , keywords    :: [Text]            -- ^ keywords
                 , pagemode    :: Maybe PageMode    -- ^ pagemode
                 , created     :: Maybe DateTime    -- ^ created date
                 , modified    :: Maybe DateTime    -- ^ modified date
                 , numberPages :: Maybe NumberPages -- ^ visible pagenumbers in pdf
                 }
          deriving (Show,Eq)

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
data Page a = Page [LayerDefinition] [ViewDefinition] [Object a]
              deriving (Eq, Show)


type LayerDefinition = Text

-- | The definition of a view
-- make active layer into an index ?
data ViewDefinition = ViewDefinition { layerNames      :: [Text]
                                     , activeLayer     :: Text
                                     }
                      deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | An ipe-object. The main ``thing'' that defines the drawings


type Attribute a = (Text,a)

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


type Matrix = ()

data CommonAttributes = CA { layer           :: Maybe LayerDefinition
                           , matrix          :: Maybe Matrix
                           , pin             :: Maybe Pin
                           , transformations :: Maybe TransformationType
                           }



data Rect a = Rect a a a a

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

data Object a = Path       CommonAttributes
                           (Maybe Color)  -- stroke
                           (Maybe Color)  -- fill
                           (Maybe SymVal) -- dash
                           (Maybe SymVal) -- pen
                           (Maybe Int) -- line cap
                           (Maybe Int) -- line join
                           (Maybe SymVal) -- fillrule
                           (Maybe Arrow) -- forward arrow
                           (Maybe Arrow) -- backward arrow
                           (Maybe SymVal) -- opaciity
                           (Maybe SymVal) -- tiling
                           (Maybe SymVal) -- tiling
                           (Maybe SymVal) -- gradient
                           [Operation a]
              | Group      CommonAttributes
                           [Operation a] -- clip
                           [Object a]
              | TextObject CommonAttributes
                           Text
              | Use        CommonAttributes
                           SymbolName
                           (P2 a)         -- pos
                           (Maybe Color)  -- stroke
                           (Maybe Color)  -- fill
                           (Maybe SymVal) -- pen
                           (Maybe SymSize) -- size
              | ImageRef   CommonAttributes
                           Int Rect
              | ImageEmbed CommonAttributes
                           Rect
                           Bitmap
                deriving (Eq,Show)






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

--------------------------------------------------------------------------------------
-- | Stuff with attributes

-- class HasAttributes c where
--     attrs      :: c -> AMap
--     updateWith :: (AMap -> AMap) -> c -> c

--     getAttr :: String -> c -> Maybe String
--     getAttr s o = M.lookup s . attrs $ o

--     setAttr     :: String -> String -> c -> c
--     setAttr k v = updateWith (M.insert k v)

--     setAttrs :: [(String,String)] -> c -> c
--     setAttrs ats = updateWith (insertAll ats)
--                    where
--                      insertAll       :: [(String,String)] -> AMap -> AMap
--                      insertAll ats m = foldr (uncurry M.insert) m ats

--     hasAttrWithValue          :: String -> String -> c -> Bool
--     hasAttrWithValue at val o = Just val == getAttr at o

--     hasAttr   :: String -> c -> Bool
--     hasAttr s = isJust . getAttr s

--     extractAttr :: String -> c -> c
--     extractAttr s = updateWith (M.delete s)




-- instance HasAttributes (IpeObject a) where
--     attrs (Path _ a)    = a
--     attrs (Group _ a)   = a
--     attrs (IpeText _ a) = a
--     attrs (Use _ a)     = a

--     updateWith f (Path ops a)  = Path ops (f a)
--     updateWith f (Group obs a) = Group obs (f a)
--     updateWith f (IpeText s a) = IpeText s (f a)
--     updateWith f (Use p a)     = Use p (f a)


--------------------------------------------------------------------------------

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
    showNum r = T.pack . show (fromRat r :: Double)
    fromSeq x  Nothing = fromInteger x
    fromSeq x (Just y) = fst . head $ readSigned readFloat (show x ++ "." ++ show y)
