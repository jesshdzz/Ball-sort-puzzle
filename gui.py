import pygame
import sys
import json
import subprocess

# --- CONFIGURACIÓN ---
ANCHO_PANTALLA = 800
ALTO_PANTALLA = 600
COLOR_FONDO = (30, 30, 30)
COLOR_TUBO = (200, 200, 200)
COLOR_SELECCION = (255, 255, 255)  # Blanco para resaltar selección

COLORES = {
    "Rojo": (231, 76, 60),
    "Verde": (46, 204, 113),
    "Azul": (52, 152, 219),
    "Amarillo": (241, 196, 15),
}

ANCHO_TUBO = 60
ALTO_TUBO = 250
RADIO_BOLA = 25
ESPACIO_ENTRE_TUBOS = 40
MARGEN_SUPERIOR = 150
MARGEN_IZQUIERDO = 100

# Nombre de tu ejecutable de Haskell
# (Como estás en Linux, asegúrate de poner ./ al principio)
EXE_HASKELL = "./ball-sort-puzzle-exe"


# --- EL PUENTE (PYTHON -> HASKELL) ---
def llamar_haskell(payload):
    """
    Envía un diccionario (payload) a Haskell como JSON y recibe la respuesta.
    """
    try:
        # json.dumps convierte el diccionario Python a texto JSON
        input_json = json.dumps(payload)

        # Ejecutamos el binario de Haskell
        # stdin=PIPE permite enviarle datos, stdout=PIPE permite leer su respuesta
        proceso = subprocess.run(
            [EXE_HASKELL], input=input_json, text=True, capture_output=True
        )

        if proceso.returncode != 0:
            print(f"Error de Haskell: {proceso.stderr}")
            return None

        # json.loads convierte el texto JSON de respuesta a un diccionario Python
        return json.loads(proceso.stdout)

    except FileNotFoundError:
        print(f"ERROR: No encuentro el archivo {EXE_HASKELL}")
        print(
            "¿Ejecutaste 'stack install --local-bin-path .'? ¿Estás en la carpeta correcta?"
        )
        sys.exit(1)


# --- LÓGICA VISUAL ---


def obtener_indice_tubo_clic(pos_mouse, num_tubos):
    """Calcula en qué tubo se hizo clic basándose en la coordenada X del mouse"""
    x_mouse, y_mouse = pos_mouse

    # Una validación simple de altura
    if not (MARGEN_SUPERIOR <= y_mouse <= MARGEN_SUPERIOR + ALTO_TUBO):
        return None

    for i in range(num_tubos):
        x_tubo = MARGEN_IZQUIERDO + (i * (ANCHO_TUBO + ESPACIO_ENTRE_TUBOS))
        if x_tubo <= x_mouse <= x_tubo + ANCHO_TUBO:
            return i
    return None


def dibujar_juego(pantalla, estado, seleccionado, mensaje):
    pantalla.fill(COLOR_FONDO)

    font = pygame.font.SysFont("Arial", 24)

    # Dibujar mensaje de estado (OK, Ganaste, Error)
    texto = font.render(f"Estado: {mensaje}", True, (255, 255, 255))
    pantalla.blit(texto, (10, 10))

    for i, tubo in enumerate(estado):
        x = MARGEN_IZQUIERDO + (i * (ANCHO_TUBO + ESPACIO_ENTRE_TUBOS))
        y = MARGEN_SUPERIOR

        # Si este tubo está seleccionado, dibujamos un borde resaltado
        color_borde = COLOR_SELECCION if i == seleccionado else COLOR_TUBO

        # Fondo del tubo
        pygame.draw.rect(
            pantalla, (50, 50, 50), (x, y, ANCHO_TUBO, ALTO_TUBO), border_radius=0
        )

        # Líneas del tubo
        grosor = 5
        pygame.draw.line(pantalla, color_borde, (x, y), (x, y + ALTO_TUBO), grosor)
        pygame.draw.line(
            pantalla,
            color_borde,
            (x + ANCHO_TUBO, y),
            (x + ANCHO_TUBO, y + ALTO_TUBO),
            grosor,
        )
        pygame.draw.line(
            pantalla,
            color_borde,
            (x, y + ALTO_TUBO),
            (x + ANCHO_TUBO, y + ALTO_TUBO),
            grosor,
        )

        # Dibujar bolas
        # Recordamos que la lista es [Arriba, ..., Abajo]
        # Pero para dibujar fácil, calculamos desde el fondo
        for j, color_nombre in enumerate(tubo):
            color_rgb = COLORES.get(color_nombre, (255, 255, 255))

            # Invertimos el índice para dibujar desde el fondo
            k = (len(tubo) - 1) - j

            centro_x = x + (ANCHO_TUBO // 2)
            centro_y = (y + ALTO_TUBO) - (k * (RADIO_BOLA * 2)) - RADIO_BOLA - 5

            pygame.draw.circle(pantalla, color_rgb, (centro_x, centro_y), RADIO_BOLA)

    pygame.display.flip()


def main():
    pygame.init()
    pantalla = pygame.display.set_mode((ANCHO_PANTALLA, ALTO_PANTALLA))
    pygame.display.set_caption("Ball Sort - Powered by Haskell")

    # --- ESTADO INICIAL ---
    # Definimos un nivel desordenado para empezar
    estado_actual = [
        ["Rojo", "Verde", "Rojo", "Azul"],
        ["Verde", "Rojo", "Verde", "Azul"],
        ["Azul", "Azul", "Rojo", "Verde"],
        [],
        [],
    ]

    seleccionado = None  # Índice del tubo seleccionado (Click 1)
    mensaje_sistema = "Juega!"
    juego_terminado = False

    reloj = pygame.time.Clock()

    while True:
        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                pygame.quit()
                sys.exit()

            if evento.type == pygame.MOUSEBUTTONDOWN and not juego_terminado:
                indice_clic = obtener_indice_tubo_clic(
                    pygame.mouse.get_pos(), len(estado_actual)
                )

                if indice_clic is not None:
                    if seleccionado is None:
                        # --- PRIMER CLIC (SELECCIONAR ORIGEN) ---
                        seleccionado = indice_clic
                        mensaje_sistema = "Selecciona destino..."
                    else:
                        # --- SEGUNDO CLIC (INTENTAR MOVER) ---
                        origen = seleccionado
                        destino = indice_clic

                        if origen == destino:
                            # Cancelar selección si clicas el mismo
                            seleccionado = None
                            mensaje_sistema = "Selección cancelada"
                        else:
                            # --- ¡AQUI LLAMAMOS A HASKELL! ---
                            peticion = {
                                "accion": "mover",
                                "estado": estado_actual,
                                "indiceDesde": origen,
                                "indiceHacia": destino,
                            }

                            print(f"Enviando a Haskell: Mover {origen} -> {destino}")
                            respuesta = llamar_haskell(peticion)

                            if respuesta:
                                mensaje_sistema = respuesta["mensaje"]
                                if respuesta["nuevoEstado"]:
                                    # Si Haskell aprobó el movimiento, actualizamos
                                    estado_actual = respuesta["nuevoEstado"]
                                    seleccionado = None  # Reseteamos selección

                                    if respuesta["esVictoria"]:
                                        mensaje_sistema = "¡GANASTE! Eres un genio."
                                        juego_terminado = True
                            else:
                                mensaje_sistema = "Error de comunicación"
                                seleccionado = None

        dibujar_juego(pantalla, estado_actual, seleccionado, mensaje_sistema)
        reloj.tick(30)


if __name__ == "__main__":
    main()
