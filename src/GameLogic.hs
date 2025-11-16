module GameLogic where

import GameTypes

tamanoTubo :: Int
tamanoTubo = 4 -- Tamaño máximo de bolas en un tubo

obtenerTubo :: EstadoJuego -> Int -> Maybe Tubo
obtenerTubo estado indice
    | indice < 0 || indice >= length estado = Nothing
    | otherwise = Just (estado !! indice)

esMovimientoValido :: EstadoJuego -> Int -> Int -> Bool
esMovimientoValido estado origen destino =
    case (obtenerTubo estado origen, obtenerTubo estado destino) of
        (Just tuboOrigen, Just tuboDestino) ->
            reglasValidas tuboOrigen tuboDestino
        _ ->
            False

reglasValidas :: Tubo -> Tubo -> Bool
reglasValidas tuboOrigen tuboDestino
    | null tuboOrigen = False -- No se puede mover de un tubo vacío
    | length tuboDestino >= tamanoTubo = False -- No se puede mover a un tubo lleno
    | null tuboDestino = True -- Se puede mover a un tubo vacío
    | otherwise =
        (head tuboOrigen) == (head tuboDestino)  -- Solo se puede mover si los colores coinciden

-- Ejecuta el movimiento, devolviendo el nuevo estado del juego
-- Se asume que el movimiento es válido
moverBola :: EstadoJuego -> Int -> Int -> EstadoJuego
moverBola estado origen destino =
    let
        -- Sacar la bola del tubo de origen
        tuboOrigen = estado !! origen
        bolaAMover = head tuboOrigen
        nuevoTuboOrigen = tail tuboOrigen

        -- Agregar la bola al tubo de destino
        tuboDestino = estado !! destino
        nuevoTuboDestino = bolaAMover : tuboDestino

        -- Actualizar el estado del juego
        -- Reemplaza el tubo en la posición dada con el nuevo tubo
        actualizarLista :: Int -> a -> [a] -> [a]
        actualizarLista idx nuevoElemento lista =
            take idx lista ++ [nuevoElemento] ++ drop (idx + 1) lista

        -- Reemplaza primero el tubo de origen y luego el de destino
        estadoIntermedio = actualizarLista origen nuevoTuboOrigen estado
        estadoFinal = actualizarLista destino nuevoTuboDestino estadoIntermedio
    in
        estadoFinal
