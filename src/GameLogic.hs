module GameLogic where

import GameTypes

tamanoTubo :: Int
tamanoTubo = 4

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
    otherwise =
        (head tuboOrigen) == (head tuboDestino)  -- Solo se puede mover si los colores coinciden
