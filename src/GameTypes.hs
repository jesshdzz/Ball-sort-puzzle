{-# LANGUAGE DeriveGeneric #-}

module GameTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Color = Rojo | Verde | Azul | Amarillo | Morado | Naranja | Cyan | Rosa
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Color
instance FromJSON Color

type Tubo = [Color]
type EstadoJuego = [Tubo]

data Peticion = Peticion {
    accion :: String,       -- "mover", "verificar", etc...
    estado :: EstadoJuego,  -- El tablero actual
    indiceDesde :: Maybe Int,    -- Puede ser null si no se requiere
    indiceHacia :: Maybe Int,    -- Puede ser null si no se requiere
    algoritmo :: Maybe String      -- "DFS", "BFS", "GREEDY", etc..., puede ser null
} deriving (Show, Generic)

instance FromJSON Peticion

data Respuesta = Respuesta {
    nuevoEstado :: Maybe EstadoJuego, -- El tablero actualizado, si aplica
    mensaje :: String,                -- "OK", "Movimiento inválido", etc...
    esVictoria :: Bool,                -- Indica si el juego se ha ganado
    solucion :: Maybe [[Int]], -- Lista de movimientos para resolver el juego, si aplica
    nodosVisitados :: Maybe Int      -- Cantidad de nodos visitados en la búsqueda, si aplica
} deriving (Show, Generic)

instance ToJSON Respuesta
