import pygame
import sys
import json
import subprocess
import random
import time

# --- CONFIGURACIÓN INICIAL DE LA VENTANA ---
ANCHO_INICIAL = 1280
ALTO_INICIAL = 720

# --- EL PUENTE (HASKELL) ---
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


# --- PALETA DE COLORES ---
COLORES = {
    "FONDO": (20, 25, 40),
    "TEXTO": (230, 230, 255),
    "TEXTO_TITULO": (100, 200, 255),
    "TEXTO_BOTON": (255, 255, 255),
    "BOTON_NORMAL": (40, 50, 80),
    "BOTON_HOVER": (70, 80, 110),
    "BOTON_ACCION": (70, 130, 180),
    "BOTON_ALGO": (100, 100, 120),
    "TUBO_VACIO": (30, 35, 50, 150),
    "TUBO_BORDE": (200, 200, 220, 180),
    "TUBO_SELECCION": (100, 255, 100),
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

DIFICULTADES = {
    "FACIL": (3, 2),
    "NORMAL": (4, 2),
    "DIFICIL": (5, 2),
    "SUPER DIFICIL": (6, 2),
    "IMPOSIBLE": (8, 2),
}

# --- CLASES ---


class Boton:
    def __init__(
        self,
        texto,
        fuente,
        padding_x=30,
        padding_y=15,
        color_base=COLORES["BOTON_NORMAL"],
    ):
        self.texto = texto
        self.fuente = fuente
        self.padding_x = padding_x
        self.padding_y = padding_y
        self.color_base = color_base
        self.actualizar_texto(texto)  # Inicializar rect y render

    def actualizar_texto(self, nuevo_texto):
        self.texto = nuevo_texto
        self.texto_render = self.fuente.render(self.texto, True, COLORES["TEXTO_BOTON"])
        ancho = self.texto_render.get_width() + self.padding_x * 2
        alto = self.texto_render.get_height() + self.padding_y * 2
        self.rect = pygame.Rect(0, 0, ancho, alto)
        self.hover = False

    def dibujar(self, pantalla, x_centro, y_centro):
        self.rect.center = (x_centro, y_centro)
        self.hover = self.rect.collidepoint(pygame.mouse.get_pos())

        r, g, b = self.color_base
        color_fondo = (
            (min(255, r + 30), min(255, g + 30), min(255, b + 30))
            if self.hover
            else self.color_base
        )

        pygame.draw.rect(pantalla, color_fondo, self.rect, border_radius=10)
        pygame.draw.rect(
            pantalla, COLORES["TEXTO_TITULO"], self.rect, width=2, border_radius=10
        )
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
    return a + (b - a) * t


class AnimacionBola:
    def __init__(self, color, pos_ini, pos_fin, duracion_seg=0.4):
        self.color = color
        self.pos_ini = pos_ini
        self.pos_fin = pos_fin
        self.duracion = duracion_seg
        self.t = 0.0
        self.terminada = False
        self.altura_arco = max(100, abs(pos_ini[0] - pos_fin[0]) * 0.3)

    def actualizar(self, dt):
        self.t += dt / self.duracion
        if self.t >= 1.0:
            self.t = 1.0
            self.terminada = True

    def get_posicion(self):
        x = lerp(self.pos_ini[0], self.pos_fin[0], self.t)
        y_lineal = lerp(self.pos_ini[1], self.pos_fin[1], self.t)
        curva_y = self.altura_arco * (4 * self.t * (1 - self.t))
        y = y_lineal - curva_y
        return (int(x), int(y))

    def dibujar(self, pantalla, radio_bola):
        x, y = self.get_posicion()
        color_rgb = COLORES["PALETA_BOLAS"].get(self.color, (255, 255, 255))
        pygame.draw.circle(pantalla, COLORES["SOMBRA_BOLA"], (x + 3, y + 3), radio_bola)
        pygame.draw.circle(pantalla, color_rgb, (x, y), radio_bola)


# --- LÓGICA DE PANTALLA ---


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


def calcular_geometria(pantalla, num_tubos):
    ancho_pantalla, alto_pantalla = pantalla.get_size()
    ancho_tubo, alto_tubo, radio_bola = 70, 300, 30
    espacio_tubo = 50
    ancho_total = (num_tubos * ancho_tubo) + ((num_tubos - 1) * espacio_tubo)

    if ancho_total > ancho_pantalla * 0.9:
        espacio_tubo = (ancho_pantalla * 0.9 - (num_tubos * ancho_tubo)) / (
            num_tubos - 1
        )
        ancho_total = ancho_pantalla * 0.9

    margen_izq = (ancho_pantalla - ancho_total) / 2
    margen_sup = (alto_pantalla - alto_tubo) / 2

    return {
        "ancho_tubo": ancho_tubo,
        "alto_tubo": alto_tubo,
        "radio_bola": radio_bola,
        "espacio_tubo": espacio_tubo,
        "margen_izq": margen_izq,
        "margen_sup": margen_sup,
    }


def get_pos_bola(geo, i_tubo, k_altura):
    x = (
        geo["margen_izq"]
        + (i_tubo * (geo["ancho_tubo"] + geo["espacio_tubo"]))
        + (geo["ancho_tubo"] / 2)
    )
    y = (geo["margen_sup"] + geo["alto_tubo"] - (geo["radio_bola"] + 10)) - (
        k_altura * (geo["radio_bola"] * 2)
    )
    return (int(x), int(y))


def get_tubo_clic(geo, pos_mouse, num_tubos):
    for i in range(num_tubos):
        x = geo["margen_izq"] + (i * (geo["ancho_tubo"] + geo["espacio_tubo"]))
        if pygame.Rect(
            x, geo["margen_sup"], geo["ancho_tubo"], geo["alto_tubo"]
        ).collidepoint(pos_mouse):
            return i
    return None


# --- DIBUJADO DE PANTALLAS ---


def dibujar_menu(pantalla, botones_menu, btn_ayuda, fuentes):
    pantalla.fill(COLORES["FONDO"])
    dibujar_texto_centrado(
        pantalla, "BALL SORT PUZZLE", fuentes["titulo"], 80, COLORES["TEXTO_TITULO"]
    )
    dibujar_texto_centrado(
        pantalla, "Haskell Core + Python UI", fuentes["subtitulo"], 150
    )

    y_base = 250
    for i, (key, boton) in enumerate(botones_menu.items()):
        boton.dibujar(pantalla, pantalla.get_width() // 2, y_base + i * 70)

    btn_ayuda.dibujar(
        pantalla, pantalla.get_width() // 2, y_base + (len(botones_menu) * 70) + 30
    )


def dibujar_ayuda(pantalla, btn_volver, fuentes):
    s = pygame.Surface(pantalla.get_size())
    s.set_alpha(240)
    s.fill((10, 10, 20))
    pantalla.blit(s, (0, 0))

    dibujar_texto_centrado(
        pantalla,
        "INSTRUCCIONES & IA EDUCATIVA",
        fuentes["titulo"],
        30,
        COLORES["TEXTO_TITULO"],
    )

    lineas = [
        "OBJETIVO: Ordena las bolas por color en los tubos.",
        "",
        "CONTROLES:",
        "• Clic Izquierdo: Mover bolas manualmente.",
        "• Botón 'Modo': Cambia el algoritmo de búsqueda (GREEDY, DFS, BFS).",
        "• Botón 'Resolver': La IA juega por ti usando el algoritmo seleccionado.",
        "",
        "COMPARACIÓN DE ALGORITMOS (EDUCATIVO):",
        "• GREEDY (Voraz): ¡Equilibrado! Usa heurísticas para encontrar",
        "  una solución rápida y buena. Recomendado.",
        "• BFS (Amplitud): ¡El Perfeccionista! Encuentra la solución MÁS CORTA (óptima),",
        "  pero es muy lento y consume mucha memoria en niveles difíciles.",
        "• DFS (Profundidad): ¡El Temerario! Muy rápido explorando, pero suele",
        "  encontrar soluciones ineficientes de cientos de pasos.",
        "",
        "Observa las estadísticas (Nodos/Tiempo) al resolver para comparar.",
    ]

    y = 100
    for linea in lineas:
        color = COLORES["TEXTO_TITULO"] if linea.endswith(":") else COLORES["TEXTO"]
        font = fuentes["boton"] if linea.endswith(":") else fuentes["normal"]
        dibujar_texto_centrado(pantalla, linea, font, y, color)
        y += 30

    btn_volver.dibujar(pantalla, pantalla.get_width() // 2, pantalla.get_height() - 60)


def dibujar_juego(
    pantalla,
    estado,
    seleccionado,
    animaciones,
    geo,
    btn_resolver,
    btn_menu,
    btn_algoritmo,
    mensaje,
    fuentes,
):
    pantalla.fill(COLORES["FONDO"])

    # Dibujar botones superiores
    btn_resolver.dibujar(pantalla, pantalla.get_width() - 120, 50)
    btn_menu.dibujar(pantalla, 120, 50)

    # Dibujar botón de algoritmo (CENTRO ARRIBA)
    btn_algoritmo.dibujar(pantalla, pantalla.get_width() // 2, 50)

    if mensaje:
        dibujar_texto_centrado(pantalla, mensaje, fuentes["normal"], 90)

    for i, tubo in enumerate(estado):
        x = geo["margen_izq"] + (i * (geo["ancho_tubo"] + geo["espacio_tubo"]))
        y_pos = geo["margen_sup"] - 30 if i == seleccionado else geo["margen_sup"]

        tubo_surf = pygame.Surface(
            (geo["ancho_tubo"], geo["alto_tubo"]), pygame.SRCALPHA
        )
        pygame.draw.rect(
            tubo_surf,
            COLORES["TUBO_VACIO"],
            (0, 0, geo["ancho_tubo"], geo["alto_tubo"]),
            border_radius=15,
        )
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

        for k, color in enumerate(tubo):
            cx, cy = get_pos_bola(geo, i, len(tubo) - 1 - k)
            pygame.draw.circle(
                pantalla, COLORES["SOMBRA_BOLA"], (cx + 3, cy + 3), geo["radio_bola"]
            )
            pygame.draw.circle(
                pantalla,
                COLORES["PALETA_BOLAS"].get(color, (255, 255, 255)),
                (cx, cy),
                geo["radio_bola"],
            )

    for anim in animaciones:
        anim.dibujar(pantalla, geo["radio_bola"])


def dibujar_victoria(pantalla, btn_menu_victoria, fuentes):
    # Dimensiones del rectángulo
    ancho, alto = pantalla.get_size()
    rect_w, rect_h = int(ancho), int(alto * 0.6)
    rect_x = (ancho - rect_w) // 2
    rect_y = (alto - rect_h) // 2

    rect_surface = pygame.Surface((rect_w, rect_h), pygame.SRCALPHA)
    rect_surface.fill(COLORES["FONDO"])
    pantalla.blit(rect_surface, (rect_x, rect_y))

    dibujar_texto_centrado(
        pantalla, "¡ NIVEL COMPLETADO !", fuentes["titulo"], 200, (100, 255, 100)
    )
    dibujar_texto_centrado(
        pantalla, "Felicidades, has completado el nivel!.", fuentes["subtitulo"], 280
    )
    btn_menu_victoria.dibujar(pantalla, pantalla.get_width() // 2, 450)


# --- MAIN LOOP ---


def main():
    pygame.init()
    pygame.font.init()
    pantalla = pygame.display.set_mode((ANCHO_INICIAL, ALTO_INICIAL), pygame.RESIZABLE)
    pygame.display.set_caption("Ball Sort - Proyecto Educativo Haskell & Python")
    reloj = pygame.time.Clock()

    fuentes = {
        "titulo": pygame.font.SysFont("Arial", 50, bold=True),
        "subtitulo": pygame.font.SysFont("Arial", 24, italic=True),
        "normal": pygame.font.SysFont("Arial", 20),
        "boton": pygame.font.SysFont("Arial", 18, bold=True),
    }

    # Botones
    botones_menu = {key: Boton(key, fuentes["boton"]) for key in DIFICULTADES.keys()}
    btn_ayuda = Boton(
        "¿CÓMO JUGAR? / EXPLICACIÓN IA",
        fuentes["boton"],
        color_base=COLORES["BOTON_ACCION"],
    )
    btn_volver_ayuda = Boton(
        "ENTENDIDO", fuentes["boton"], color_base=COLORES["BOTON_ACCION"]
    )

    btn_resolver = Boton(
        "Resolver IA", fuentes["boton"], color_base=COLORES["BOTON_ACCION"]
    )
    btn_menu = Boton("Menú", fuentes["boton"])

    algoritmo_actual = "GREEDY"
    btn_algoritmo = Boton(
        f"Modo: {algoritmo_actual}", fuentes["boton"], color_base=COLORES["BOTON_ALGO"]
    )

    btn_menu_victoria = Boton(
        "Volver al Menú Principal", fuentes["boton"], padding_x=40
    )

    estado_app = "MENU"
    estado_juego, seleccionado, animaciones, estado_futuro, cola_solucion = (
        [],
        None,
        [],
        None,
        [],
    )
    victoria_pendiente, mensaje = False, ""

    corriendo = True
    while corriendo:
        dt = reloj.tick(60) / 1000.0
        pos_mouse = pygame.mouse.get_pos()

        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                corriendo = False
            if evento.type == pygame.VIDEORESIZE:
                pass

            if evento.type == pygame.MOUSEBUTTONDOWN:
                if estado_app == "MENU":
                    if btn_ayuda.es_clic(pos_mouse):
                        estado_app = "AYUDA"
                    else:
                        for key, boton in botones_menu.items():
                            if boton.es_clic(pos_mouse):
                                estado_juego = generar_nivel(key)
                                (
                                    seleccionado,
                                    animaciones,
                                    estado_futuro,
                                    cola_solucion,
                                ) = None, [], None, []
                                victoria_pendiente, mensaje, estado_app = (
                                    False,
                                    "¡Suerte!",
                                    "JUEGO",
                                )

                elif estado_app == "AYUDA":
                    if btn_volver_ayuda.es_clic(pos_mouse):
                        estado_app = "MENU"

                elif estado_app == "GANASTE":
                    if btn_menu_victoria.es_clic(pos_mouse):
                        estado_app = "MENU"

                elif estado_app == "JUEGO" and not animaciones:
                    if btn_menu.es_clic(pos_mouse):
                        estado_app = "MENU"

                    # LOGICA CAMBIO ALGORITMO
                    elif btn_algoritmo.es_clic(pos_mouse):
                        modos = ["GREEDY", "DFS", "BFS"]
                        idx = modos.index(algoritmo_actual)
                        algoritmo_actual = modos[(idx + 1) % len(modos)]
                        btn_algoritmo.actualizar_texto(f"Modo: {algoritmo_actual}")

                    elif btn_resolver.es_clic(pos_mouse):
                        mensaje = f"Ejecutando {algoritmo_actual} en Haskell..."
                        geo = calcular_geometria(pantalla, len(estado_juego))
                        dibujar_juego(
                            pantalla,
                            estado_juego,
                            seleccionado,
                            animaciones,
                            geo,
                            btn_resolver,
                            btn_menu,
                            btn_algoritmo,
                            mensaje,
                            fuentes,
                        )
                        pygame.display.flip()

                        start_time = time.time()
                        # Enviamos el algoritmo seleccionado
                        res = llamar_haskell(
                            {
                                "accion": "resolver",
                                "estado": estado_juego,
                                "indiceDesde": None,
                                "indiceHacia": None,
                                "algoritmo": algoritmo_actual,
                            }
                        )
                        end_time = time.time()

                        if res and res.get("solucion"):
                            cola_solucion = res["solucion"]
                            nodos = res.get("nodosVisitados", "?")
                            pasos = len(cola_solucion)
                            tiempo = round(end_time - start_time, 4)
                            mensaje = f"Solución: {pasos} pasos | Nodos: {nodos} | Tiempo: {tiempo}s"
                        else:
                            nodos = res.get("nodosVisitados", "?")
                            mensaje = f"Falló o límite excedido (Nodos: {nodos})"

                    else:
                        # Lógica de juego manual
                        geo = calcular_geometria(pantalla, len(estado_juego))
                        tubo_clic = get_tubo_clic(geo, pos_mouse, len(estado_juego))
                        if tubo_clic is not None:
                            if seleccionado is None:
                                if estado_juego[tubo_clic]:
                                    seleccionado = tubo_clic
                            elif seleccionado == tubo_clic:
                                seleccionado = None
                            else:
                                res = llamar_haskell(
                                    {
                                        "accion": "mover",
                                        "estado": estado_juego,
                                        "indiceDesde": seleccionado,
                                        "indiceHacia": tubo_clic,
                                        "algoritmo": algoritmo_actual,
                                    }
                                )
                                if res and res["nuevoEstado"]:
                                    estado_futuro = res["nuevoEstado"]
                                    victoria_pendiente = res["esVictoria"]

                                    origen, destino = seleccionado, tubo_clic
                                    num_movidas = len(estado_juego[origen]) - len(
                                        estado_futuro[origen]
                                    )
                                    color_movido = estado_juego[origen][0]
                                    estado_juego[origen] = estado_juego[origen][
                                        num_movidas:
                                    ]
                                    for k in range(num_movidas):
                                        pos_ini = get_pos_bola(
                                            geo,
                                            origen,
                                            len(estado_juego[origen])
                                            + num_movidas
                                            - 1
                                            - k,
                                        )
                                        pos_fin = get_pos_bola(
                                            geo, destino, len(estado_juego[destino]) + k
                                        )
                                        animaciones.append(
                                            AnimacionBola(
                                                color_movido, pos_ini, pos_fin
                                            )
                                        )
                                    seleccionado = None
                                else:
                                    mensaje, seleccionado = "Movimiento inválido", None

        # UPDATE LOOP
        if estado_app == "JUEGO":
            if cola_solucion and not animaciones:
                # Extraer siguiente paso de la solución
                origen, destino = cola_solucion.pop(0)

                # Ejecutar movimiento visualmente
                res = llamar_haskell(
                    {
                        "accion": "mover",
                        "estado": estado_juego,
                        "indiceDesde": origen,
                        "indiceHacia": destino,
                        "algoritmo": algoritmo_actual,
                    }
                )
                if res and res["nuevoEstado"]:
                    estado_futuro = res["nuevoEstado"]
                    victoria_pendiente = res["esVictoria"]

                    num_movidas = len(estado_juego[origen]) - len(estado_futuro[origen])
                    color_movido = estado_juego[origen][0]
                    estado_juego[origen] = estado_juego[origen][num_movidas:]

                    for k in range(num_movidas):
                        geo = calcular_geometria(pantalla, len(estado_juego))
                        h_origen = len(estado_juego[origen]) + num_movidas - 1 - k
                        h_destino = len(estado_juego[destino]) + k
                        pos_ini = get_pos_bola(geo, origen, h_origen)
                        pos_fin = get_pos_bola(geo, destino, h_destino)
                        animaciones.append(
                            AnimacionBola(
                                color_movido, pos_ini, pos_fin, duracion_seg=0.6
                            )
                        )

            if animaciones:
                todo_ok = True
                for a in animaciones:
                    a.actualizar(dt)
                    if not a.terminada:
                        todo_ok = False
                if todo_ok:
                    animaciones, estado_juego, estado_futuro = [], estado_futuro, None
                    if victoria_pendiente:
                        estado_app, victoria_pendiente = "GANASTE", False

        # DRAW LOOP
        if estado_app == "MENU":
            dibujar_menu(pantalla, botones_menu, btn_ayuda, fuentes)
        elif estado_app == "AYUDA":
            dibujar_ayuda(pantalla, btn_volver_ayuda, fuentes)
        elif estado_app == "GANASTE":
            dibujar_victoria(pantalla, btn_menu_victoria, fuentes)
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
                btn_algoritmo,
                mensaje,
                fuentes,
            )

        pygame.display.flip()

    pygame.quit()
    sys.exit()


if __name__ == "__main__":
    main()
