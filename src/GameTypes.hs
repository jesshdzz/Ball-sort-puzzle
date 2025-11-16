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

data Peticion = Peticion {
    accion :: String,       -- "mover", "verificar", etc...
    estado :: EstadoJuego,  -- El tablero actual
    indiceDesde :: Maybe Int,    -- Puede ser null si no se requiere
    indiceHacia :: Maybe Int    -- Puede ser null si no se requiere
} deriving (Show, Generic)

instance FromJSON Peticion

data Respuesta = Respuesta {
    nuevoEstado :: Maybe EstadoJuego, -- El tablero actualizado, si aplica
    mensaje :: String,                -- "OK", "Movimiento inválido", etc...
    esVictoria :: Bool                -- Indica si el juego se ha ganado
} deriving (Show, Generic)

instance ToJSON Respuesta
