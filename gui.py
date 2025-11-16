import pygame
import sys
import json
import subprocess
import random
import math

# --- CONFIGURACIÓN VISUAL ---
ANCHO_PANTALLA = 900
ALTO_PANTALLA = 700
COLOR_FONDO = (20, 25, 40)  # Azul noche oscuro
COLOR_TUBO = (200, 200, 220)
COLOR_TUBO_SELECCION = (100, 255, 100)  # Verde neón
COLOR_TEXTO = (255, 255, 255)

# Paleta de colores bonita para las bolas
PALETA_BOLAS = {
    "Rojo": (255, 89, 94),  # Red
    "Verde": (138, 201, 38),  # Green
    "Azul": (25, 130, 196),  # Blue
    "Amarillo": (255, 202, 58),  # Yellow
    "Morado": (106, 76, 147),  # Purple
    "Naranja": (251, 133, 0),  # Orange
    "Cyan": (76, 201, 240),  # Cyan
    "Rosa": (255, 0, 110),  # Pink
}

# Geometría
ANCHO_TUBO = 70
ALTO_TUBO = 300
RADIO_BOLA = 30
ESPACIO_ENTRE_TUBOS = 50
MARGEN_SUPERIOR = 200

EXE_HASKELL = "./ball-sort-puzzle-exe"

# --- NIVELES DE DIFICULTAD ---
# (Colores, Tubos Vacíos Extra)
DIFICULTADES = {
    "FACIL": (3, 2),  # 3 colores, 2 vacíos (Total 5 tubos)
    "NORMAL": (4, 2),  # 4 colores, 2 vacíos (Total 6 tubos)
    "DIFICIL": (5, 2),  # 5 colores, 2 vacíos (Total 7 tubos)
    "SUPER DIFICIL": (6, 2),  # 6 colores, 2 vacíos (Total 8 tubos)
    "IMPOSIBLE": (8, 2),  # 8 colores, 2 vacíos (Total 10 tubos)
}

# --- CLASES ---


class AnimacionBola:
    def __init__(self, color, inicio_pos, fin_pos, velocidad=15):
        self.color = color
        self.x, self.y = inicio_pos
        self.dest_x, self.dest_y = fin_pos
        self.velocidad = velocidad
        self.terminada = False

        # Calcular vector de dirección
        dx = self.dest_x - self.x
        dy = self.dest_y - self.y
        distancia = math.sqrt(dx**2 + dy**2)
        self.vx = (dx / distancia) * velocidad
        self.vy = (dy / distancia) * velocidad
        self.distancia_total = distancia
        self.distancia_recorrida = 0

    def actualizar(self):
        self.x += self.vx
        self.y += self.vy
        self.distancia_recorrida += self.velocidad

        if self.distancia_recorrida >= self.distancia_total:
            self.x = self.dest_x
            self.y = self.dest_y
            self.terminada = True

    def dibujar(self, pantalla):
        pygame.draw.circle(
            pantalla,
            PALETA_BOLAS.get(self.color, (255, 255, 255)),
            (int(self.x), int(self.y)),
            RADIO_BOLA,
        )


# --- FUNCIONES DE APOYO ---


def generar_nivel(dificultad_key):
    num_colores, tubos_vacios = DIFICULTADES[dificultad_key]
    nombres_colores = list(PALETA_BOLAS.keys())[:num_colores]

    # Crear todas las bolas necesarias (4 de cada color)
    todas_bolas = []
    for color in nombres_colores:
        todas_bolas.extend([color] * 4)

    random.shuffle(todas_bolas)

    # Repartir en tubos
    tubos = []
    for i in range(num_colores):
        inicio = i * 4
        tubos.append(todas_bolas[inicio : inicio + 4])

    # Añadir tubos vacíos
    for _ in range(tubos_vacios):
        tubos.append([])

    return tubos


def llamar_haskell(payload):
    try:
        input_json = json.dumps(payload)
        proceso = subprocess.run(
            [EXE_HASKELL], input=input_json, text=True, capture_output=True
        )
        if proceso.returncode != 0:
            return None
        return json.loads(proceso.stdout)
    except FileNotFoundError:
        return None


def calcular_posicion_bola(idx_tubo, altura_en_tubo, num_tubos):
    ancho_total = (num_tubos * ANCHO_TUBO) + ((num_tubos - 1) * ESPACIO_ENTRE_TUBOS)
    margen_izquierdo = (ANCHO_PANTALLA - ancho_total) // 2

    x = (
        margen_izquierdo
        + (idx_tubo * (ANCHO_TUBO + ESPACIO_ENTRE_TUBOS))
        + (ANCHO_TUBO // 2)
    )

    # Altura en tubo: 0 es fondo, 3 es tope.
    # Dibujamos desde abajo: Y_BASE - (altura * diametro) - radio
    y = (
        (MARGEN_SUPERIOR + ALTO_TUBO)
        - (altura_en_tubo * (RADIO_BOLA * 2))
        - RADIO_BOLA
        - 10
    )
    return (x, y)


# --- PANTALLAS ---


def dibujar_menu(pantalla, fuente_titulo, fuente_btn):
    pantalla.fill(COLOR_FONDO)

    # Título con sombra
    titulo = fuente_titulo.render("BALL SORT PUZZLE", True, (100, 200, 255))
    pantalla.blit(titulo, (ANCHO_PANTALLA // 2 - titulo.get_width() // 2, 100))

    botones = []
    y_base = 250

    for i, dif in enumerate(DIFICULTADES.keys()):
        rect = pygame.Rect(ANCHO_PANTALLA // 2 - 150, y_base + (i * 70), 300, 50)
        color_btn = (50, 60, 80)

        # Efecto hover simple
        if rect.collidepoint(pygame.mouse.get_pos()):
            color_btn = (70, 80, 110)

        pygame.draw.rect(pantalla, color_btn, rect, border_radius=10)
        pygame.draw.rect(pantalla, (100, 200, 255), rect, width=2, border_radius=10)

        texto = fuente_btn.render(dif, True, COLOR_TEXTO)
        pantalla.blit(
            texto,
            (
                rect.centerx - texto.get_width() // 2,
                rect.centery - texto.get_height() // 2,
            ),
        )

        botones.append((rect, dif))

    return botones


def dibujar_juego(pantalla, estado, seleccionado, animaciones):
    pantalla.fill(COLOR_FONDO)
    num_tubos = len(estado)
    ancho_total = (num_tubos * ANCHO_TUBO) + ((num_tubos - 1) * ESPACIO_ENTRE_TUBOS)
    margen_izq = (ANCHO_PANTALLA - ancho_total) // 2

    # Dibujar tubos y bolas estáticas
    for i, tubo in enumerate(estado):
        x = margen_izq + (i * (ANCHO_TUBO + ESPACIO_ENTRE_TUBOS))
        y = MARGEN_SUPERIOR

        color_borde = COLOR_TUBO_SELECCION if i == seleccionado else COLOR_TUBO

        # Tubo (Cuerpo)
        rect_tubo = pygame.Rect(x, y, ANCHO_TUBO, ALTO_TUBO)
        pygame.draw.rect(
            pantalla,
            (30, 35, 50),
            rect_tubo,
            border_radius=0,
            border_bottom_left_radius=20,
            border_bottom_right_radius=20,
        )

        # Bordes
        pygame.draw.line(
            pantalla, color_borde, (x, y), (x, y + ALTO_TUBO - 20), 4
        )  # Izq
        pygame.draw.line(
            pantalla,
            color_borde,
            (x + ANCHO_TUBO, y),
            (x + ANCHO_TUBO, y + ALTO_TUBO - 20),
            4,
        )  # Der
        # Curva abajo (simulada con arco o líneas)
        pygame.draw.line(
            pantalla,
            color_borde,
            (x, y + ALTO_TUBO),
            (x + ANCHO_TUBO, y + ALTO_TUBO),
            4,
        )  # Base simple por ahora

        # Bolas estáticas
        # Invertimos para dibujar desde el fondo (0 es fondo)
        # tubo_invertido = list(reversed(tubo))
        # Pero ojo: mi función 'calcular_posicion_bola' usa índice desde el fondo (0 = fondo)
        # En Haskell la lista es [Tope ... Fondo].
        # Así que el elemento en index K de haskell, está a una altura (len - 1 - k)

        num_bolas = len(tubo)
        for k, color in enumerate(tubo):
            altura = num_bolas - 1 - k
            cx, cy = calcular_posicion_bola(i, altura, num_tubos)
            pygame.draw.circle(pantalla, PALETA_BOLAS[color], (cx, cy), RADIO_BOLA)

    # Dibujar bolas animadas (encima de todo)
    for anim in animaciones:
        anim.dibujar(pantalla)


# --- MAIN LOOP ---


def main():
    pygame.init()
    pantalla = pygame.display.set_mode((ANCHO_PANTALLA, ALTO_PANTALLA))
    pygame.display.set_caption("Ball Sort - Haskell Core")
    reloj = pygame.time.Clock()

    fuente_titulo = pygame.font.SysFont("Arial", 60, bold=True)
    fuente_btn = pygame.font.SysFont("Arial", 30)

    estado_app = "MENU"  # MENU, JUEGO, GANASTE

    # Variables de juego
    estado_juego = []
    seleccionado = None
    animaciones = []  # Lista de objetos AnimacionBola
    estado_futuro = None  # Para guardar el estado mientras animamos
    mensaje = ""
    victoria = False

    corriendo = True
    while corriendo:
        pos_mouse = pygame.mouse.get_pos()

        if estado_app == "MENU":
            botones = dibujar_menu(pantalla, fuente_titulo, fuente_btn)

            for evento in pygame.event.get():
                if evento.type == pygame.QUIT:
                    corriendo = False
                if evento.type == pygame.MOUSEBUTTONDOWN:
                    for rect, dif in botones:
                        if rect.collidepoint(pos_mouse):
                            estado_juego = generar_nivel(dif)
                            seleccionado = None
                            animaciones = []
                            estado_futuro = None
                            mensaje = ""
                            estado_app = "JUEGO"

        elif estado_app == "JUEGO":
            # Lógica de actualización de animaciones
            if animaciones:
                todas_terminadas = True
                for anim in animaciones:
                    anim.actualizar()
                    if not anim.terminada:
                        todas_terminadas = False

                if todas_terminadas:
                    animaciones = []
                    estado_juego = estado_futuro
                    estado_futuro = None

                    if victoria:
                        estado_app = "GANASTE"
                        victoria = False
            # Dibujado
            dibujar_juego(pantalla, estado_juego, seleccionado, animaciones)

            # Mensajes
            if mensaje:
                txt = fuente_btn.render(mensaje, True, (200, 200, 200))
                pantalla.blit(txt, (20, 20))

            # Eventos
            for evento in pygame.event.get():
                if evento.type == pygame.QUIT:
                    corriendo = False

                # Bloquear input si hay animaciones
                if evento.type == pygame.MOUSEBUTTONDOWN and not animaciones:
                    # Detectar clic en tubos
                    num_tubos = len(estado_juego)
                    ancho_total = (num_tubos * ANCHO_TUBO) + (
                        (num_tubos - 1) * ESPACIO_ENTRE_TUBOS
                    )
                    margen_izq = (ANCHO_PANTALLA - ancho_total) // 2

                    clic_en_tubo = None
                    for i in range(num_tubos):
                        x = margen_izq + (i * (ANCHO_TUBO + ESPACIO_ENTRE_TUBOS))
                        if x <= pos_mouse[0] <= x + ANCHO_TUBO:
                            if (
                                MARGEN_SUPERIOR
                                <= pos_mouse[1]
                                <= MARGEN_SUPERIOR + ALTO_TUBO
                            ):
                                clic_en_tubo = i
                                break

                    if clic_en_tubo is not None:
                        if seleccionado is None:
                            if estado_juego[clic_en_tubo]:  # Si no está vacío
                                seleccionado = clic_en_tubo
                        else:
                            origen = seleccionado
                            destino = clic_en_tubo

                            if origen == destino:
                                seleccionado = None
                            else:
                                # --- LLAMADA A HASKELL ---
                                peticion = {
                                    "accion": "mover",
                                    "estado": estado_juego,
                                    "indiceDesde": origen,
                                    "indiceHacia": destino,
                                }
                                res = llamar_haskell(peticion)

                                if res and res["nuevoEstado"]:
                                    # MOVIEMIENTO VALIDO: INICIAR ANIMACIÓN
                                    estado_futuro = res["nuevoEstado"]

                                    victoria = res["esVictoria"]

                                    # Calcular cuántas bolas y de qué color se mueven
                                    # Comparamos longitudes para saber cuantas salieron
                                    len_antes = len(estado_juego[origen])
                                    len_despues = len(estado_futuro[origen])
                                    num_bolas_movidas = len_antes - len_despues
                                    color_movido = estado_juego[origen][
                                        0
                                    ]  # La de arriba

                                    # Crear animaciones para cada bola movida
                                    for k in range(num_bolas_movidas):
                                        # Posicion inicial: Tubo Origen, altura (len_antes - 1 - k)
                                        pos_ini = calcular_posicion_bola(
                                            origen, len_antes - 1 - k, num_tubos
                                        )

                                        # Posicion final: Tubo Destino, altura (len_destino_actual + k)
                                        len_dest_actual = len(estado_juego[destino])
                                        pos_fin = calcular_posicion_bola(
                                            destino, len_dest_actual + k, num_tubos
                                        )

                                        anim = AnimacionBola(
                                            color_movido, pos_ini, pos_fin
                                        )
                                        animaciones.append(anim)

                                    # Eliminamos temporalmente las bolas del origen VISUALMENTE (hack visual)
                                    # Para que no se dupliquen mientras viajan.
                                    # Modificamos una copia temporal solo para dibujar el fondo?
                                    # Nah, simplemente dibujamos las animaciones encima.
                                    # Pero idealmente deberíamos quitar las bolas estáticas.
                                    # Truco rapido: Actualizar estado_juego INMEDIATAMENTE en el origen
                                    # pero NO en el destino hasta que termine la animacion?
                                    # Mejor: Dejamos el estado como está, y pintamos un cuadrado del color de fondo
                                    # sobre las bolas que se están moviendo en el origen :P
                                    # O mejor: Simplemente actualizamos estado_juego a estado_futuro AL FINAL.
                                    # (Se verán duplicadas por un milisegundo, pero con la velocidad no se nota mucho)

                                    # REFINAMIENTO:
                                    # Quitamos las bolas del origen en 'estado_juego' ahora mismo
                                    # pero NO las ponemos en destino todavía.
                                    estado_juego[origen] = estado_juego[origen][
                                        num_bolas_movidas:
                                    ]
                                    # (Las bolas están ahora "en el aire" en la lista 'animaciones')

                                    seleccionado = None
                                    mensaje = ""
                                else:
                                    mensaje = "Movimiento invalido"
                                    seleccionado = None

        elif estado_app == "GANASTE":
            pantalla.fill(COLOR_FONDO)
            txt = fuente_titulo.render("¡VICTORIA!", True, (100, 255, 100))
            pantalla.blit(txt, (ANCHO_PANTALLA // 2 - txt.get_width() // 2, 200))

            btn_menu = pygame.Rect(ANCHO_PANTALLA // 2 - 100, 400, 200, 50)
            pygame.draw.rect(pantalla, (50, 60, 80), btn_menu, border_radius=10)
            txt_btn = fuente_btn.render("Menú Principal", True, COLOR_TEXTO)
            pantalla.blit(
                txt_btn,
                (
                    btn_menu.centerx - txt_btn.get_width() // 2,
                    btn_menu.centery - txt_btn.get_height() // 2,
                ),
            )

            for evento in pygame.event.get():
                if evento.type == pygame.QUIT:
                    corriendo = False
                if evento.type == pygame.MOUSEBUTTONDOWN:
                    if btn_menu.collidepoint(pos_mouse):
                        estado_app = "MENU"

        pygame.display.flip()
        reloj.tick(60)  # 60 FPS para animaciones fluidas

    pygame.quit()
    sys.exit()


if __name__ == "__main__":
    main()
