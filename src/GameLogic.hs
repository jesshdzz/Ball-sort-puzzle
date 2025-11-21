module GameLogic (esMovimientoValido, moverBola, estaResuelto, resolver) where

import GameTypes
import Data.List (nub, sort, sortBy)
import qualified Data.Set as Set
import Data.Ord (comparing)

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

-- Puntúa qué tan "desordenado" está un tablero. Más bajo es mejor.
heuristic :: EstadoJuego -> Int
heuristic = sum . map puntuarTubo
  where
    -- Puntúa un solo tubo
    puntuarTubo :: Tubo -> Int
    puntuarTubo [] = 0 -- Un tubo vacío está "resuelto" (puntuación 0)
    puntuarTubo tubo = 
        case reverse tubo of -- [Fondo, ... Tope]
            [] -> 0 -- Imposible, pero exhaustivo
            (colorFondo:resto) -> 
                -- Contamos cuántas bolas en 'resto' NO son del 'colorFondo'
                length (filter (/= colorFondo) resto)

resolver :: EstadoJuego -> Maybe [(Int, Int)]
resolver estadoInicial = 
    let 
        -- 1. Normalizamos y preparamos
        estadoInicialNorm = sort estadoInicial
        visitadosInicial = Set.fromList [estadoInicialNorm]
        
        -- 2. La cola inicial (EstadoReal, HistorialDeMovimientos)
        colaInicial = [(estadoInicial, [])]
    in 
        -- 3. Iniciar la búsqueda
        bfs colaInicial visitadosInicial
  where
    -- bfs :: ColaPrioridad -> Visitados -> Solución
    bfs :: [(EstadoJuego, [(Int, Int)])] -> Set.Set EstadoJuego -> Maybe [(Int, Int)]
    bfs [] _ = Nothing -- Se acabó la cola, no hay solución
    
    -- El bucle principal de búsqueda
    bfs cola visitados =
        let 
            -- Ordenamos la cola por heurística.
            -- El estado con puntuación más baja se pone primero.
            colaOrdenada = sortBy (comparing (heuristic . fst)) cola
            
            -- Sacamos el "mejor" estado para procesar
            (actual, historial) = head colaOrdenada
            restoCola = tail colaOrdenada
        in
        
        -- Saber si hay solución
        if estaResuelto actual then
            Just (reverse historial) -- Solución encontrada
        else
            let
                -- Generar todos los movimientos vecinos
                vecinos = movimientosPosibles actual
                
                -- 5. Preparar los nuevos estados para la cola
                -- (EstadoReal, NuevoHistorial, EstadoNormalizado)
                nuevosEstados = [ (eReal, (d,h):historial, sort eReal) | (eReal, (d,h)) <- vecinos ]
                
                -- 6. Filtrar los que ya hemos visitado
                estadosParaAnadir = filter (\(_, _, eNorm) -> not (Set.member eNorm visitados)) nuevosEstados
                
                -- 7. Añadir los nuevos estados a la cola y a visitados
                colaNueva = restoCola ++ map (\(eR, h, _) -> (eR, h)) estadosParaAnadir
                visitadosNuevos = Set.union visitados (Set.fromList (map (\(_,_,eN) -> eN) estadosParaAnadir))
                
            in
                -- 8. Llamada recursiva con la nueva cola y visitados
                bfs colaNueva visitadosNuevos
