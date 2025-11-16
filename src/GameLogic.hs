module GameLogic (esMovimientoValido, moverBola, estaResuelto, resolver) where

import GameTypes
import Data.List (nub, sort)
import qualified Data.Set as Set

tamanoTubo :: Int
tamanoTubo = 4 -- Tamaño máximo de bolas en un tubo

obtenerTubo :: EstadoJuego -> Int -> Maybe Tubo
obtenerTubo tablero indice
    | indice < 0 || indice >= length tablero = Nothing
    | otherwise = Just (tablero !! indice)

esMovimientoValido :: EstadoJuego -> Int -> Int -> Bool
esMovimientoValido tablero origen destino =
    case (obtenerTubo tablero origen, obtenerTubo tablero destino) of
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
        let colorOrigen = head tuboOrigen
            colorDestino = head tuboDestino
        in colorOrigen == colorDestino -- Solo se puede mover si los colores coinciden

-- Ejecuta el movimiento, devolviendo el nuevo estado del juego
-- Se asume que el movimiento es válido
moverBola :: EstadoJuego -> Int -> Int -> EstadoJuego
moverBola tablero origen destino =
    let
        tuboOrigen = tablero !! origen -- Sacar la bola del tubo de origen
        tuboDestino = tablero !! destino
        
        colorMover = head tuboOrigen

        bolasDisponibles = length (takeWhile (== colorMover) tuboOrigen)
        espacioDisponible = tamanoTubo - length tuboDestino
        cantidadAMover = min bolasDisponibles espacioDisponible

        (bolasAMover, nuevoTuboOrigen) = splitAt cantidadAMover tuboOrigen
    
        -- Agregar la bola al tubo de destino
        nuevoTuboDestino = bolasAMover ++ tuboDestino

        -- Actualizar el estado del juego
        -- Reemplaza el tubo en la posición dada con el nuevo tubo
        actualizarLista :: Int -> a -> [a] -> [a]
        actualizarLista idx nuevoElemento lista =
            take idx lista ++ [nuevoElemento] ++ drop (idx + 1) lista

        -- Reemplaza primero el tubo de origen y luego el de destino
        estadoIntermedio = actualizarLista origen nuevoTuboOrigen tablero
        estadoFinal = actualizarLista destino nuevoTuboDestino estadoIntermedio
    in
        estadoFinal

esTuboResuelto :: Tubo -> Bool
esTuboResuelto tubo
    | null tubo = True -- Un tubo vacío se considera resuelto
    | length tubo < tamanoTubo = False -- Un tubo no lleno no está resuelto
    | otherwise =
        case tubo of
            (primero:resto) -> all (== primero) resto
            []              -> True

estaResuelto :: EstadoJuego -> Bool
estaResuelto tablero =
    all esTuboResuelto tablero

-- Solucionador
-- Lista de todos los movimientos posibles desde el estado actual
movimientosPosibles :: EstadoJuego -> [(EstadoJuego, (Int, Int))]
movimientosPosibles tablero =
    let 
        indices = [0 .. length tablero - 1]
        pares = [(i, j) | i <- indices, j <- indices, i /= j]
    in 
        [ (moverBola tablero i j, (i, j)) 
        | (i, j) <- pares, esMovimientoValido tablero i j]

-- Algoritmo BFS para encontrar la solución
resolver :: EstadoJuego -> Maybe [(Int, Int)]
resolver estadoInicial = 
    let 
        -- 1. Normalizamos el estado inicial
        estadoInicialNorm = sort estadoInicial
        -- 2. Creamos nuestro Set de visitados, con el estado inicial ya dentro
        visitadosInicial = Set.fromList [estadoInicialNorm]
    in 
        -- 3. Iniciamos la cola con el estado REAL
        bfs [(estadoInicial, [])] visitadosInicial
  where
    -- bfs :: Cola(EstadoReal, Historial) -> Visitados(Set[EstadoNormalizado]) -> Solución
    bfs :: [(EstadoJuego, [(Int, Int)])] -> Set.Set EstadoJuego -> Maybe [(Int, Int)]
    bfs [] _ = Nothing -- Cola vacía, no hay solución
    bfs ((actual, historial):restoCola) visitados
        | estaResuelto actual = Just (reverse historial) -- ¡Éxito!
        | otherwise = 
            let 
                -- 1. Generar todos los movimientos desde el estado REAL
                siguientes = movimientosPosibles actual
                
                -- 2. Normalizar los nuevos estados
                siguientesNorm = map (\(e, m) -> (sort e, (e, m))) siguientes
                
                -- 3. Filtrar los que ya hemos visitado (¡USANDO Set.member!)
                -- 'Set.member' es ultra-rápido comparado con 'elem'
                nuevos = filter (\(eNorm, _) -> not (Set.member eNorm visitados)) siguientesNorm
                
                -- 4. Añadir los estados REALES (eReal) a la cola
                colaNueva = restoCola ++ [ (eReal, (d, h) : historial) | (_, (eReal, (d, h))) <- nuevos ]
                
                -- 5. Añadir los nuevos estados NORMALIZADOS al Set de visitados
                visitadosNuevos = Set.union visitados (Set.fromList (map fst nuevos))
            in 
                bfs colaNueva visitadosNuevos
