{-# LANGUAGE DeriveGeneric #-}

module GameTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Color = Rojo | Verde | Azul | Amarillo
    deriving (Show, Eq, Generic)

instance ToJSON Color
instance FromJSON Color

type Tubo = [Color]
type EstadoJuego = [Tubo]
