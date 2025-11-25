module GameLogic (esMovimientoValido, moverBola, estaResuelto, resolver) where

import GameTypes
import Data.List (nub, sort, sortBy)
import qualified Data.Set as Set
import Data.Ord (comparing)

tamanoTubo :: Int
tamanoTubo = 4 -- Tamaño máximo de bolas en un tubo

type NodoBusqueda = (EstadoJuego, [(Int, Int)]) -- (EstadoActual, HistorialDeMovimientos)

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
            [] -> 0
            (colorFondo:resto) -> 
                -- Contamos cuántas bolas en 'resto' NO son del 'colorFondo'
                length (filter (/= colorFondo) resto)

resolver :: EstadoJuego -> String -> Maybe ([(Int, Int)], Int)
resolver estadoInicial nombreAlgoritmo = 
    let 
        estadoInicialNorm = sort estadoInicial
        visitadosInicial = Set.fromList [estadoInicialNorm]
        colaInicial = [(estadoInicial, [])]
        -- Iniciamos con 0 nodos visitados
    in 
        busquedaGenerica colaInicial visitadosInicial 0 nombreAlgoritmo

-- Función que cambia su comportamiento según el algoritmo
busquedaGenerica :: [NodoBusqueda] -> Set.Set EstadoJuego -> Int -> String -> Maybe ([(Int, Int)], Int)
busquedaGenerica [] _ _ _ = Nothing
busquedaGenerica cola visitados contador alg
    | contador > 200000 = Nothing -- Límite de seguridad
    | otherwise =
        let 
            -- 1. ELEGIR EL SIGUIENTE NODO
            -- extrae ((Estado, Historial), RestoDeCola)
            ((estadoActual, pasos), restoCola) = case alg of
                "DFS" -> (head cola, tail cola) -- Toma el primero (LIFO behavior al insertar al inicio)
                "BFS" -> (head cola, tail cola) -- Toma el primero (FIFO behavior al insertar al final)
                "GREEDY" -> 
                     -- Ordenamos y sacamos el mejor
                     let colaOrdenada = sortBy (comparing (heuristic . fst)) cola
                     in (head colaOrdenada, tail colaOrdenada)
                _ -> (head cola, tail cola)

        in
            if estaResuelto estadoActual then
                Just (reverse pasos, contador) -- Éxito
            else
                let
                    vecinos = movimientosPosibles estadoActual
                    
                    -- Generar nuevos nodos: (Estado, NuevoHistorial)
                    nuevosNodos = [ (eReal, (d,h):pasos) | (eReal, (d,h)) <- vecinos ]
                    
                    -- Filtrar visitados
                    nodosValidos = filter (\(e, _) -> not (Set.member (sort e) visitados)) nuevosNodos
                    
                    -- Actualizar Visitados
                    nuevosVisitados = Set.union visitados (Set.fromList (map (sort . fst) nodosValidos))
                    
                    -- 2. COMBINAR LA COLA
                    nuevaCola = case alg of
                        "DFS" -> nodosValidos ++ restoCola      -- Pone los nuevos AL PRINCIPIO
                        "BFS" -> restoCola ++ nodosValidos      -- Pone los nuevos AL FINAL
                        "GREEDY" -> restoCola ++ nodosValidos   -- (El ordenamiento ocurre al sacar)
                        _ -> restoCola ++ nodosValidos
                in
                    busquedaGenerica nuevaCola nuevosVisitados (contador + 1) alg
