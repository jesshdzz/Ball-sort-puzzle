# Ball Sort Puzzle (Haskell + Python)

Este es un proyecto universitario para la materia de **Programación Funcional**, cuyo objetivo principal es presentar una arquitectura de sistema híbrida donde **Haskell** actúa como el "cerebro" lógico y **Python** actúa como la interfaz de usuario.

El proyecto es una implementación del popular juego "Ball Sort Puzzle", con un menú de dificultad y un solver de IA capaz de resolver todos los niveles.

---

## Características

* **Jugabilidad:** Lógica de juego que sigue todas las reglas (movimiento, victoria, etc.).
* **Movimiento en Bloque:** Mueve automáticamente todas las bolas del mismo color consecutivas que quepan en el tubo de destino.
* **Interfaz Gráfica:** Creada con Pygame, la interfaz es totalmente responsiva y se centra automáticamente si se redimensiona la ventana.
* **Menú de Dificultad:** Incluye 5 niveles (de Fácil a Imposible) que ajustan el número de colores y tubos.
* **Animaciones:** Las bolas realizan un arco parabólico suave al moverse entre tubos.
* **Solver de IA** Un potente solver capaz de encontrar la solución para cualquier tablero.

---

## Arquitectura del Sistema

Está dividido en dos componentes que se comunican en tiempo real:

### 1. El Cerebro (Haskell)

* Es un ejecutable de línea de comandos (`ball-sort-puzzle-exe`) compilado con `Stack`.
* No tiene estado. Recibe el estado actual del juego en formato JSON.
* Procesa una acción (`"mover"`, `"resolver"`).
* Devuelve el nuevo estado del juego o la solución en formato JSON.
* Contiene **toda la lógica del juego**, reglas de movimiento y el algoritmo de la IA.

### 2. La Interfaz (Python)

* Es un script de Pygame (`gui.py`) que se ejecuta en un entorno virtual.
* No sabe las reglas del juego. Su único trabajo es "pintar" el estado que recibe.
* Cuando el usuario hace clic, empaqueta la intención (ej. "mover de 3 a 5") y el estado actual en un JSON.
* Llama al ejecutable de Haskell usando `subprocess` y le pasa el JSON por `stdin`.
* Lee la respuesta de Haskell por `stdout`, la decodifica y actualiza la pantalla (o anima la solución).

---

## Stack Tecnológico

### Backend (Lógica Central)

* **Haskell**
  * `Stack`: Para la gestión de dependencias y compilación.
  * `aeson`: Para la serialización/deserialización de JSON.
  * `containers (Set)`: Para la optimización de estados visitados en el solver.

### Frontend (Interfaz de Usuario)

* **Python 3**
  * `pygame`: Para la creación de la ventana, gráficos, animaciones y manejo de eventos.
  * `venv`: Para la gestión de un entorno aislado.

---

## Cómo Ejecutarlo

El proyecto requiere compilar el backend de Haskell primero, y luego ejecutar el frontend de Python.

### Paso 1: Compilar el Backend (Haskell)

> [!IMPORTANT]
> Tener [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) instalado.

```bash
# 1. Navega a la raíz del proyecto
# (Donde está el archivo stack.yaml)

# 2. Compila el proyecto y sus dependencias
stack build

# 3. Instala el ejecutable en la raíz del proyecto
# (Esto crea el archivo 'ball-sort-puzzle-exe')
stack install --local-bin-path .
```

### Paso 2: Ejecutar el Frontend (Python)

> [!IMPORTANT]
> Tener `python3` y `pip` instalados.

```Bash
# 1. (Opcional pero recomendado) Crea y activa un entorno virtual
python3 -m venv venv
source venv/bin/activate  # (En Windows: .\venv\Scripts\activate)

# 2. Instala la única dependencia (pygame)
pip install pygame

# 3. Ejecuta el juego
python gui.py
```

## El Solver IA

Un requisito clave era crear un solver que funcionara en niveles difíciles. Un simple BFS (Búsqueda en Amplitud) fallaría al explorar miles de millones de estados inútiles.La solución implementada es un Greedy Best-First Search (Búsqueda "Codiciosa").

### 1. La Heurística (La "Intuición")

Para que el solver sea "inteligente", le dimos una "función heurística" que le permite "puntuar" qué tan "ordenado" está un tablero.
La puntuación se calcula así:

> Puntuación = Total de bolas que no coinciden con el color de la bola del fondo de su tubo.

* Un tubo `[Rojo, Rojo, Rojo]` (fondo: Rojo) tiene una puntuación de 0.
* Un tubo `[Verde, Azul, Rojo, Rojo]` (fondo: Rojo) tiene una puntuación de 2 (Verde y Azul están "mal").

El algoritmo siempre da prioridad a explorar el movimiento que resulta en el tablero con la puntuación más baja.

### 2. Optimización (Evitar Bucles)

Para evitar que la IA se quede atascada en bucles (ej. A->B, B->A), utilizamos dos técnicas:

1. Normalización de Estados: Antes de guardar un estado como "visitado", lo "normalizamos" usando `sort`. Esto le enseña a la IA que un tablero con `[TuboA, TuboB]` es idéntico a `[TuboB, TuboA]`, reduciendo drásticamente el número de estados a explorar.
2. `Data.Set`: Usamos un `Set` (Conjunto) de Haskell en lugar de una lista para guardar los estados visitados. Comprobar si un estado existe en un Set es casi instantáneo ($O(\log n)$), mientras que en una lista es extremadamente lento ($O(n)$).

Esta combinación de Heurística + Normalización + Set es lo que permite al solver encontrar soluciones a niveles "Imposibles" en segundos, en lugar de años.
