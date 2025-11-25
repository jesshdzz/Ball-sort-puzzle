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
            let errorResp = Respuesta Nothing "Error: JSON inválido" False Nothing Nothing
            B.putStrLn (encode errorResp)
        Just peticion -> do
            procesarPeticion peticion

procesarPeticion :: Peticion -> IO ()
procesarPeticion p
    | accion p == "mover" = manejarMovimiento p
    | accion p == "resolver" = manejarResolver p
    | otherwise = do
        let resp = Respuesta Nothing "Error: Acción desconocida" False Nothing Nothing
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
                resp = Respuesta (Just estadoNuevo) msg victoria Nothing Nothing
            B.putStrLn (encode resp)
        else do
            -- movimiento inválido
            let resp = Respuesta Nothing "Movimiento inválido" False Nothing Nothing
            B.putStrLn (encode resp)

manejarResolver :: Peticion -> IO ()
manejarResolver p = do
    let estadoActual = estado p
    -- Leemos el algoritmo, por defecto "GREEDY"
    let nombreAlg = fromMaybe "GREEDY" (algoritmo p)
 
    case resolver estadoActual nombreAlg of
        Just (pasos, nodos) -> do
            let pasosLista = map (\(d, h) -> [d, h]) pasos
            -- Enviamos la solución Y el conteo de nodos
            let resp = Respuesta Nothing "Solución Encontrada" False (Just pasosLista) (Just nodos)
            B.putStrLn (encode resp)
        Nothing -> do
            let resp = Respuesta Nothing "Límite excedido o sin solución" False Nothing (Just 0)
            B.putStrLn (encode resp)
