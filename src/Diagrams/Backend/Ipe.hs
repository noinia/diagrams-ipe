{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module Diagrams.Backend.Ipe where



import Data.Typeable

import Diagrams.Prelude



data Ipe = Ipe
           deriving (Show,Eq,Typeable)



instance Monoid (Render Ipe (V2 b)) where
  mempty = R $ return mempty
  (R r1) `mappend` (R r2_) =
    R $ do
      svg1 <- r1
      svg2 <- r2_
      return (svg1 `mappend` svg2)

type IpeRenderer = ()

type IpeDocument

data IpeOptions = IpeOptions

instance Backend Ipe (V2 b) where
    type Render  Ipe (V2 b) = R IpeRenderer
    type Result  Ipe (V2 b) = IpeDocument
    type Options Ipe (V2 b) = IpeOptions

    withStyle s t (R r) = R $ do
                                --TODO set all style attributes
                                return r

    doRender _ opts (R r) = undefined -- TODO: somehow produce the ipe output
