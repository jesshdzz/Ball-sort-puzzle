import pygame
import sys
import json
import subprocess
import random
import math

# --- CONFIGURACIÓN INICIAL DE LA VENTANA ---
# La ventana ahora es REAJUSTABLE
ANCHO_INICIAL = 1280
ALTO_INICIAL = 720

# --- EL PUENTE (SIN CAMBIOS) ---
EXE_HASKELL = "./ball-sort-puzzle-exe"


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


# --- PALETA DE COLORES MODERNA ---
COLORES = {
    "FONDO": (20, 25, 40),  # Azul noche
    "TEXTO": (230, 230, 255),
    "TEXTO_TITULO": (100, 200, 255),
    "TEXTO_BOTON": (255, 255, 255),
    "BOTON_NORMAL": (40, 50, 80),
    "BOTON_HOVER": (70, 80, 110),
    "TUBO_VACIO": (30, 35, 50, 150),  # Semi-transparente
    "TUBO_BORDE": (200, 200, 220, 180),
    "TUBO_SELECCION": (100, 255, 100),  # Verde neón
    "SOMBRA_BOLA": (0, 0, 0, 50),
    "PALETA_BOLAS": {
        "Rojo": (255, 89, 94),
        "Verde": (138, 201, 38),
        "Azul": (25, 130, 196),
        "Amarillo": (255, 202, 58),
        "Morado": (106, 76, 147),
        "Naranja": (251, 133, 0),
        "Cyan": (76, 201, 240),
        "Rosa": (255, 0, 110),
    },
}

# --- NIVELES DE DIFICULTAD (SIN CAMBIOS) ---
DIFICULTADES = {
    "FACIL": (3, 2),
    "NORMAL": (4, 2),
    "DIFICIL": (5, 2),
    "SUPER DIFICIL": (6, 2),
    "IMPOSIBLE": (8, 2),
}


# --- CLASE BOTÓN RESPONSIVO ---
# Esta clase crea un botón que se ajusta al tamaño de su texto
class Boton:
    def __init__(self, texto, fuente, padding_x=30, padding_y=15):
        self.texto = texto
        self.fuente = fuente
        self.padding_x = padding_x
        self.padding_y = padding_y
        self.texto_render = self.fuente.render(self.texto, True, COLORES["TEXTO_BOTON"])

        # El tamaño se calcula dinámicamente
        ancho = self.texto_render.get_width() + self.padding_x * 2
        alto = self.texto_render.get_height() + self.padding_y * 2
        self.rect = pygame.Rect(0, 0, ancho, alto)
        self.hover = False

    def dibujar(self, pantalla, x_centro, y_centro):
        self.rect.center = (x_centro, y_centro)

        # Detección de hover
        self.hover = self.rect.collidepoint(pygame.mouse.get_pos())
        color_fondo = COLORES["BOTON_HOVER"] if self.hover else COLORES["BOTON_NORMAL"]

        pygame.draw.rect(pantalla, color_fondo, self.rect, border_radius=10)
        # Borde sutil
        pygame.draw.rect(
            pantalla, COLORES["TEXTO_TITULO"], self.rect, width=2, border_radius=10
        )

        # Texto centrado dentro del botón
        pantalla.blit(
            self.texto_render,
            (
                self.rect.centerx - self.texto_render.get_width() // 2,
                self.rect.centery - self.texto_render.get_height() // 2,
            ),
        )

    def es_clic(self, pos_mouse):
        return self.rect.collidepoint(pos_mouse)


def lerp(a, b, t):
    """Interpolación lineal simple"""
    return a + (b - a) * t


# --- CLASE DE ANIMACIÓN MEJORADA (CON PARÁBOLA) ---
class AnimacionBola:
    def __init__(self, color, pos_ini, pos_fin, duracion_seg=0.4):
        self.color = color
        self.pos_ini = pos_ini
        self.pos_fin = pos_fin
        self.duracion = duracion_seg
        self.t = 0.0  # Tiempo de animación (0.0 a 1.0)
        self.terminada = False

        # Altura del arco (en píxeles, sobre el punto más alto)
        self.altura_arco = max(100, abs(pos_ini[0] - pos_fin[0]) * 0.3)

    def actualizar(self, dt):  # dt = delta_time en segundos
        self.t += dt / self.duracion
        if self.t >= 1.0:
            self.t = 1.0
            self.terminada = True

    def get_posicion(self):
        # Interpolación lineal (lerp) para X
        x = lerp(self.pos_ini[0], self.pos_fin[0], self.t)

        # Interpolación lineal para Y...
        y_lineal = lerp(self.pos_ini[1], self.pos_fin[1], self.t)
        # ...más una parábola que es 0 en t=0 y t=1
        # La fórmula 4*t*(1-t) da una curva parabólica perfecta entre 0 y 1
        curva_y = self.altura_arco * (4 * self.t * (1 - self.t))

        y = y_lineal - curva_y
        return (int(x), int(y))

    def dibujar(self, pantalla, radio_bola):
        x, y = self.get_posicion()
        color_rgb = COLORES["PALETA_BOLAS"].get(self.color, (255, 255, 255))

        # Sombra
        pygame.draw.circle(pantalla, COLORES["SOMBRA_BOLA"], (x + 3, y + 3), radio_bola)
        # Bola
        pygame.draw.circle(pantalla, color_rgb, (x, y), radio_bola)


# --- FUNCIONES AUXILIARES ---


def generar_nivel(dificultad_key):
    num_colores, tubos_vacios = DIFICULTADES[dificultad_key]
    nombres_colores = list(COLORES["PALETA_BOLAS"].keys())[:num_colores]
    todas_bolas = [color for color in nombres_colores for _ in range(4)]
    random.shuffle(todas_bolas)
    tubos = [todas_bolas[i * 4 : (i + 1) * 4] for i in range(num_colores)]
    tubos.extend([[] for _ in range(tubos_vacios)])
    return tubos


def dibujar_texto_centrado(pantalla, texto, fuente, y, color=COLORES["TEXTO"]):
    render = fuente.render(texto, True, color)
    x = pantalla.get_width() // 2 - render.get_width() // 2
    pantalla.blit(render, (x, y))


# --- CÁLCULO DE GEOMETRÍA DINÁMICO ---
# Esta es la clave para la responsividad
def calcular_geometria(pantalla, num_tubos):
    ancho_pantalla = pantalla.get_width()
    alto_pantalla = pantalla.get_height()

    # Tamaños base de los tubos (podrían escalar, pero es más fácil fijos)
    ancho_tubo = 70
    alto_tubo = 300
    radio_bola = 30

    # Ajustar espaciado si no caben
    espacio_tubo = 50
    ancho_total = (num_tubos * ancho_tubo) + ((num_tubos - 1) * espacio_tubo)

    # Si se sale de la pantalla, reducimos el espaciado
    if ancho_total > ancho_pantalla * 0.9:
        espacio_tubo = (ancho_pantalla * 0.9 - (num_tubos * ancho_tubo)) / (
            num_tubos - 1
        )
        ancho_total = ancho_pantalla * 0.9

    # CENTRADO DINÁMICO
    margen_izq = (ancho_pantalla - ancho_total) / 2
    margen_sup = (alto_pantalla - alto_tubo) / 2  # Centrado vertical

    return {
        "ancho_tubo": ancho_tubo,
        "alto_tubo": alto_tubo,
        "radio_bola": radio_bola,
        "espacio_tubo": espacio_tubo,
        "margen_izq": margen_izq,
        "margen_sup": margen_sup,
    }


def get_pos_bola(geo, i_tubo, k_altura):
    # k_altura: 0 = fondo, 3 = tope
    x = (
        geo["margen_izq"]
        + (i_tubo * (geo["ancho_tubo"] + geo["espacio_tubo"]))
        + (geo["ancho_tubo"] / 2)
    )
    y_base_tubo = (
        geo["margen_sup"] + geo["alto_tubo"] - (geo["radio_bola"] + 10)
    )  # 10px padding fondo
    y = y_base_tubo - (k_altura * (geo["radio_bola"] * 2))
    return (int(x), int(y))


def get_tubo_clic(geo, pos_mouse, num_tubos):
    for i in range(num_tubos):
        x_tubo = geo["margen_izq"] + (i * (geo["ancho_tubo"] + geo["espacio_tubo"]))
        y_tubo = geo["margen_sup"]
        rect_tubo = pygame.Rect(x_tubo, y_tubo, geo["ancho_tubo"], geo["alto_tubo"])
        if rect_tubo.collidepoint(pos_mouse):
            return i
    return None


# --- PANTALLAS ---


def dibujar_menu(pantalla, botones_menu, fuentes):
    pantalla.fill(COLORES["FONDO"])
    dibujar_texto_centrado(
        pantalla, "BALL SORT PUZZLE", fuentes["titulo"], 100, COLORES["TEXTO_TITULO"]
    )
    dibujar_texto_centrado(
        pantalla, "(Haskell Core + Python UI)", fuentes["subtitulo"], 180
    )

    y_base = 300
    for i, (key, boton) in enumerate(botones_menu.items()):
        boton.dibujar(pantalla, pantalla.get_width() // 2, y_base + i * 80)


def dibujar_juego(
    pantalla, estado, seleccionado, animaciones, geo, btn_resolver, btn_menu, mensaje
):
    pantalla.fill(COLORES["FONDO"])

    # Dibujar botones
    btn_resolver.dibujar(pantalla, pantalla.get_width() - 120, 50)
    btn_menu.dibujar(pantalla, 120, 50)

    # Dibujar mensaje de estado
    if mensaje:
        dibujar_texto_centrado(pantalla, mensaje, fuentes["normal"], 30)

    for i, tubo in enumerate(estado):
        x = geo["margen_izq"] + (i * (geo["ancho_tubo"] + geo["espacio_tubo"]))

        # Animación de "pop-up" al seleccionar
        y_base = geo["margen_sup"]
        y_pos = y_base - 30 if i == seleccionado else y_base

        rect_tubo = pygame.Rect(x, y_pos, geo["ancho_tubo"], geo["alto_tubo"])

        # Tubo (hecho con Surface para transparencia)
        tubo_surf = pygame.Surface(
            (geo["ancho_tubo"], geo["alto_tubo"]), pygame.SRCALPHA
        )
        pygame.draw.rect(
            tubo_surf,
            COLORES["TUBO_VACIO"],
            (0, 0, geo["ancho_tubo"], geo["alto_tubo"]),
            border_radius=15,
        )

        # Bordes
        color_borde = (
            COLORES["TUBO_SELECCION"] if i == seleccionado else COLORES["TUBO_BORDE"]
        )
        pygame.draw.rect(
            tubo_surf,
            color_borde,
            (0, 0, geo["ancho_tubo"], geo["alto_tubo"]),
            width=4,
            border_radius=15,
        )

        pantalla.blit(tubo_surf, (x, y_pos))

        # Bolas estáticas
        num_bolas = len(tubo)
        for k_haskell, color in enumerate(tubo):
            k_altura = num_bolas - 1 - k_haskell  # 0 = fondo
            cx, cy = get_pos_bola(geo, i, k_altura)
            color_rgb = COLORES["PALETA_BOLAS"].get(color, (255, 255, 255))

            # Sombra
            pygame.draw.circle(
                pantalla, COLORES["SOMBRA_BOLA"], (cx + 3, cy + 3), geo["radio_bola"]
            )
            # Bola
            pygame.draw.circle(pantalla, color_rgb, (cx, cy), geo["radio_bola"])

    # Dibujar bolas animadas (encima de todo)
    for anim in animaciones:
        anim.dibujar(pantalla, geo["radio_bola"])


def dibujar_victoria(pantalla, btn_menu_victoria, fuentes):
    pantalla.fill(COLORES["FONDO"])
    dibujar_texto_centrado(
        pantalla,
        "¡ N I V E L   C O M P L E T A D O !",
        fuentes["titulo"],
        200,
        (100, 255, 100),
    )
    btn_menu_victoria.dibujar(pantalla, pantalla.get_width() // 2, 400)


# --- BUCLE PRINCIPAL ---


def main():
    pygame.init()
    pygame.font.init()

    # Permitir reajustar la ventana
    pantalla = pygame.display.set_mode((ANCHO_INICIAL, ALTO_INICIAL), pygame.RESIZABLE)
    pygame.display.set_caption("Ball Sort Puzzle - Haskell Core (Diseño Dinámico)")
    reloj = pygame.time.Clock()

    # Fuentes
    global fuentes  # Hacerlas globales para que las usen las funciones de dibujado
    fuentes = {
        "titulo": pygame.font.SysFont("Arial", 60, bold=True),
        "subtitulo": pygame.font.SysFont("Arial", 24, italic=True),
        "normal": pygame.font.SysFont("Arial", 24),
        "boton": pygame.font.SysFont("Arial", 22, bold=True),
    }

    # Crear botones del menú
    botones_menu = {key: Boton(key, fuentes["boton"]) for key in DIFICULTADES.keys()}

    # Crear botones del juego
    btn_resolver = Boton("Resolver IA", fuentes["boton"], padding_x=20, padding_y=10)
    btn_menu = Boton("Volver al Menú", fuentes["boton"], padding_x=20, padding_y=10)
    btn_menu_victoria = Boton("Menú Principal", fuentes["boton"])

    estado_app = "MENU"

    # Variables de juego
    estado_juego = []
    seleccionado = None
    animaciones = []
    estado_futuro = None
    victoria_pendiente = False
    cola_solucion = []
    mensaje = ""

    corriendo = True
    while corriendo:
        # dt = delta_time en segundos. Esencial para animaciones suaves.
        dt = reloj.tick(60) / 1000.0
        pos_mouse = pygame.mouse.get_pos()

        # --- MANEJO DE EVENTOS ---
        eventos = pygame.event.get()
        for evento in eventos:
            if evento.type == pygame.QUIT:
                corriendo = False

            # Re-dibujar si la ventana cambia de tamaño
            if evento.type == pygame.VIDEORESIZE:
                # El sistema maneja el reajuste, solo necesitamos redibujar
                pass

            if evento.type == pygame.MOUSEBUTTONDOWN:
                if estado_app == "MENU":
                    for key, boton in botones_menu.items():
                        if boton.es_clic(pos_mouse):
                            estado_juego = generar_nivel(key)
                            # Resetear todo
                            seleccionado = None
                            animaciones = []
                            estado_futuro = None
                            victoria_pendiente = False
                            cola_solucion = []
                            mensaje = "¡Suerte!"
                            estado_app = "JUEGO"

                elif (
                    estado_app == "JUEGO" and not animaciones
                ):  # Bloquear clics si hay animación
                    if btn_resolver.es_clic(pos_mouse):
                        mensaje = "IA pensando... (puede tardar)"
                        # Forzar redibujado del mensaje
                        geo = calcular_geometria(pantalla, len(estado_juego))
                        dibujar_juego(
                            pantalla,
                            estado_juego,
                            seleccionado,
                            animaciones,
                            geo,
                            btn_resolver,
                            btn_menu,
                            mensaje,
                        )
                        pygame.display.flip()

                        peticion = {
                            "accion": "resolver",
                            "estado": estado_juego,
                            "indiceDesde": None,
                            "indiceHacia": None,
                        }
                        res = llamar_haskell(peticion)

                        if res and res.get("solucionesPosibles"):
                            cola_solucion = res["solucionesPosibles"]
                            mensaje = f"Solución encontrada: {len(cola_solucion)} pasos"
                        else:
                            mensaje = "La IA no encontró solución"

                    elif btn_menu.es_clic(pos_mouse):
                        estado_app = "MENU"

                    else:
                        geo = calcular_geometria(pantalla, len(estado_juego))
                        clic_en_tubo = get_tubo_clic(geo, pos_mouse, len(estado_juego))

                        if clic_en_tubo is not None:
                            if seleccionado is None:
                                if estado_juego[clic_en_tubo]:  # Si no está vacío
                                    seleccionado = clic_en_tubo
                                    mensaje = ""
                            else:
                                if seleccionado == clic_en_tubo:
                                    seleccionado = None  # Cancelar
                                else:
                                    # --- LLAMADA A HASKELL ---
                                    origen = seleccionado
                                    destino = clic_en_tubo
                                    peticion = {
                                        "accion": "mover",
                                        "estado": estado_juego,
                                        "indiceDesde": origen,
                                        "indiceHacia": destino,
                                    }
                                    res = llamar_haskell(peticion)

                                    if res and res["nuevoEstado"]:
                                        estado_futuro = res["nuevoEstado"]
                                        victoria_pendiente = res["esVictoria"]

                                        # Calcular bolas movidas para animar
                                        len_antes = len(estado_juego[origen])
                                        len_despues = len(estado_futuro[origen])
                                        num_bolas_movidas = len_antes - len_despues
                                        color_movido = estado_juego[origen][0]

                                        # Quitar bolas del origen lógico INMEDIATAMENTE
                                        estado_juego[origen] = estado_juego[origen][
                                            num_bolas_movidas:
                                        ]

                                        # Crear animaciones
                                        for k in range(num_bolas_movidas):
                                            # Altura en origen (desde el fondo)
                                            altura_ini = (
                                                len_antes - num_bolas_movidas + k
                                            )
                                            pos_ini = get_pos_bola(
                                                geo, origen, altura_ini
                                            )

                                            # Altura en destino (desde el fondo)
                                            altura_fin = len(estado_juego[destino]) + k
                                            pos_fin = get_pos_bola(
                                                geo, destino, altura_fin
                                            )

                                            anim = AnimacionBola(
                                                color_movido, pos_ini, pos_fin
                                            )
                                            animaciones.append(anim)

                                        seleccionado = None
                                    else:
                                        mensaje = "Movimiento inválido"
                                        seleccionado = None

                elif estado_app == "GANASTE":
                    if btn_menu_victoria.es_clic(pos_mouse):
                        estado_app = "MENU"

        # --- LÓGICA DE ACTUALIZACIÓN ---
        if estado_app == "JUEGO":
            # Ejecutar cola del solver si existe
            if cola_solucion and not animaciones:
                origen, destino = cola_solucion.pop(0)

                # (Duplicamos la lógica de movimiento, idealmente iría en una función)
                geo = calcular_geometria(pantalla, len(estado_juego))
                peticion = {
                    "accion": "mover",
                    "estado": estado_juego,
                    "indiceDesde": origen,
                    "indiceHacia": destino,
                }
                res = llamar_haskell(peticion)

                if res and res["nuevoEstado"]:
                    estado_futuro = res["nuevoEstado"]
                    victoria_pendiente = res["esVictoria"]
                    len_antes = len(estado_juego[origen])
                    len_despues = len(estado_futuro[origen])
                    num_bolas_movidas = len_antes - len_despues
                    color_movido = estado_juego[origen][0]
                    estado_juego[origen] = estado_juego[origen][num_bolas_movidas:]

                    for k in range(num_bolas_movidas):
                        altura_ini = len_antes - num_bolas_movidas + k
                        pos_ini = get_pos_bola(geo, origen, altura_ini)
                        altura_fin = len(estado_juego[destino]) + k
                        pos_fin = get_pos_bola(geo, destino, altura_fin)
                        anim = AnimacionBola(
                            color_movido, pos_ini, pos_fin, duracion_seg=0.25
                        )  # Solver más rápido
                        animaciones.append(anim)

            # Actualizar animaciones existentes
            if animaciones:
                todas_terminadas = True
                for anim in animaciones:
                    anim.actualizar(dt)
                    if not anim.terminada:
                        todas_terminadas = False

                if todas_terminadas:
                    animaciones = []
                    estado_juego = estado_futuro
                    estado_futuro = None
                    if victoria_pendiente:
                        estado_app = "GANASTE"
                        victoria_pendiente = False

        # --- DIBUJADO ---
        if estado_app == "MENU":
            dibujar_menu(pantalla, botones_menu, fuentes)

        elif estado_app == "JUEGO":
            geo = calcular_geometria(pantalla, len(estado_juego))
            dibujar_juego(
                pantalla,
                estado_juego,
                seleccionado,
                animaciones,
                geo,
                btn_resolver,
                btn_menu,
                mensaje,
            )

        elif estado_app == "GANASTE":
            dibujar_victoria(pantalla, btn_menu_victoria, fuentes)

        pygame.display.flip()

    pygame.quit()
    sys.exit()


if __name__ == "__main__":
    main()

