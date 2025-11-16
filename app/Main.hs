{-# LANGUAGE OverloadedStrings #-}

module Main where

import GameTypes
import GameLogic
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    -- Leer la entrada JSON desde stdin
    input <- B.getContents
    -- Decodificar la entrada JSON a una Peticion
    let posiblePeticion = decode input :: Maybe Peticion

    case posiblePeticion of
        Nothing -> do
            -- Si la decodificación falla, responder con un error
            let errorResp = Respuesta Nothing "Error: JSON inválido" False
            B.putStrLn (encode errorResp)
        Just peticion -> do
            procesarPeticion peticion

procesarPeticion :: Peticion -> IO ()
procesarPeticion p
    | accion p == "mover" = manejarMovimiento p
    | otherwise = do
        let resp = Respuesta Nothing "Error: Acción desconocida" False
        B.putStrLn (encode resp)

manejarMovimiento :: Peticion -> IO ()
manejarMovimiento p = do
    let estadoActual = estado p
        desde = fromMaybe (-1) (indiceDesde p)
        hacia = fromMaybe (-1) (indiceHacia p)

    if esMovimientoValido estadoActual desde hacia
        then do
            let estadoNuevo = moverBola estadoActual desde hacia
                victoria = estaResuelto estadoNuevo
                msg = if victoria then "Ganaste!" else "OK"
                resp = Respuesta (Just estadoNuevo) msg victoria
            B.putStrLn (encode resp)
        else do
            -- movimiento inválido
            let resp = Respuesta Nothing "Movimiento inválido" False
            B.putStrLn (encode resp)
