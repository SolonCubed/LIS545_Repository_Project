## =========================================================

## Replication Data: Aranda, D., Sáenz-Leandro, R., & Fernández-de-Castro, P. (2026). Gamblificación y patrones de diseño persuasivo 
## en los videojuegos: resultados de una encuesta a jóvenes en España

## =========================================================




## =========================================================
## 0. Paquetes
## =========================================================
library(tibble)
library(dplyr)
library(ggplot2)

## Helper para IC 95%
add_ci <- function(df) {
  df %>%
    mutate(
      ci_low  = b - 1.96 * se,
      ci_high = b + 1.96 * se
    )
}

## =========================================================
## 1. FIGURA 1 – Exposición gamblificada (M1a, M1b)
##    DV: Afinidad azar / Gasto azar
## =========================================================

coef_M1 <- tribble(
  ~model, ~predictor, ~label,              ~b,       ~se,
  "M1a",  "A1",      "Tiempo semanal",    -0.021,   0.026,
  "M1a",  "A5",      "Gasto mensual",      0.130,   0.014,
  "M1a",  "F1",      "Edad",              -0.010,   0.010,
  "M1b",  "A1",      "Tiempo semanal",     0.010,   0.035,
  "M1b",  "A5",      "Gasto mensual",      0.375,   0.019,
  "M1b",  "F1",      "Edad",               0.003,   0.013
)

coef_M1 <- coef_M1 %>%
  mutate(
    model = factor(model, levels = c("M1a","M1b"),
                   labels = c("DV: Afinidad azar", "DV: Gasto azar"))
  ) %>%
  add_ci() %>%
  # Orden de predictores en el eje Y
  mutate(label = factor(label,
                        levels = rev(c("Gasto mensual","Tiempo semanal","Edad"))))

fig1_M1 <- ggplot(coef_M1,
                  aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(~ model) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Exposición gamblificada: tiempo, gasto y afinidad/gasto azar"
  ) +
  theme_light()

## Para ver la figura:
fig1_M1



## =========================================================
## 2. FIGURA 2 – Patrones oscuros (M2a, M2b)
##    DV: Patrones temporales / Patrones monetarios
## =========================================================

coef_M2 <- tribble(
  ~model, ~predictor,        ~label,                 ~b,      ~se,
  "M2a",  "afinidad_azar",   "Afinidad azar",        0.190,  0.027,
  "M2a",  "gasto_azar",      "Gasto azar",           0.243,  0.020,
  "M2a",  "A1",              "Tiempo semanal",       0.129,  0.021,
  "M2a",  "A5",              "Gasto mensual",        0.021,  0.014,
  "M2a",  "F7",              "Situación laboral",   -0.070,  0.036,
  "M2b",  "afinidad_azar",   "Afinidad azar",        0.104,  0.025,
  "M2b",  "gasto_azar",      "Gasto azar",           0.273,  0.019,
  "M2b",  "A1",              "Tiempo semanal",       0.049,  0.020,
  "M2b",  "A5",              "Gasto mensual",        0.065,  0.013,
  "M2b",  "F7",              "Situación laboral",   -0.074,  0.033
)

coef_M2 <- coef_M2 %>%
  mutate(
    model = factor(model, levels = c("M2a","M2b"),
                   labels = c("DV: Patrones temporales", "DV: Patrones monetarios"))
  ) %>%
  add_ci() %>%
  mutate(label = factor(label,
                        levels = rev(c("Gasto azar","Afinidad azar",
                                       "Gasto mensual","Tiempo semanal",
                                       "Situación laboral"))))

fig2_M2 <- ggplot(coef_M2,
                  aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(~ model) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Predictores de patrones oscuros temporales y monetarios"
  ) +
  theme_light()

## Para ver la figura:
fig2_M2



## =========================================================
## 3. FIGURA 3 – Impacto emocional (M3a vs M4b)
##    M3a: sin conciencia crítica / M4b: con conciencia crítica e interacciones
## =========================================================

coef_M3_M4b <- tribble(
  # M3a: impacto_emocional ~ patrones + afinidad/gasto azar + A1 + A5 + F1
  ~model, ~predictor,             ~label,                   ~b,      ~se,
  "M3a",  "patrones_temporales",   "Patrones temporales",   0.364,  0.037,
  "M3a",  "patrones_monetarios",   "Patrones monetarios",   0.667,  0.042,
  "M3a",  "afinidad_azar",         "Afinidad azar",        -0.049,  0.025,
  "M3a",  "gasto_azar",            "Gasto azar",           -0.037,  0.021,
  
  # M4b: impacto_emocional ~ patrones_c + conc_critica_c + interacciones + A1 + A5 + F1
  "M4b",  "patrones_temporales_c", "Patrones temporales (c)", 0.336, 0.033,
  "M4b",  "patrones_monetarios_c", "Patrones monetarios (c)", 0.596, 0.034,
  "M4b",  "conciencia_critica_c",  "Conciencia crítica (c)",  0.113, 0.033,
  "M4b",  "int_temp_conc",         "Interacción temp×conc",   0.044, 0.043,
  "M4b",  "int_mon_conc",          "Interacción mon×conc",    0.000, 0.042
)

coef_M3_M4b <- coef_M3_M4b %>%
  mutate(
    model = factor(model, levels = c("M3a","M4b"),
                   labels = c("M3a: sin conciencia crítica", "M4b: con conciencia crítica e interacciones"))
  ) %>%
  add_ci() %>%
  mutate(label = factor(label,
                        levels = rev(c("Patrones monetarios","Patrones temporales",
                                       "Afinidad azar","Gasto azar",
                                       "Patrones monetarios (c)","Patrones temporales (c)",
                                       "Conciencia crítica (c)",
                                       "Interacción temp×conc","Interacción mon×conc"))))

fig3_impacto <- ggplot(coef_M3_M4b,
                       aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(~ model) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Patrones oscuros, conciencia crítica e impacto emocional"
  ) +
  theme_light()

## Para ver la figura:
fig3_impacto



## =========================================================
## 4. FIGURA 4 – Conciencia crítica (M4a)
##    DV: Conciencia crítica
## =========================================================

coef_M4a <- tribble(
  ~predictor,            ~label,                  ~b,      ~se,
  "afinidad_azar",       "Afinidad azar",        -0.211,  0.030,
  "gasto_azar",          "Gasto azar",           -0.110,  0.023,
  "patrones_temporales", "Patrones temporales",   0.112,  0.038,
  "patrones_monetarios", "Patrones monetarios",   0.428,  0.046,
  "A1",                  "Tiempo semanal",        0.068,  0.023,
  "A5",                  "Gasto mensual",         0.001,  0.015,
  "F1",                  "Edad",                 -0.001,  0.011
)

coef_M4a <- coef_M4a %>%
  add_ci() %>%
  mutate(label = factor(label,
                        levels = rev(c("Patrones monetarios","Patrones temporales",
                                       "Tiempo semanal","Gasto mensual",
                                       "Afinidad azar","Gasto azar","Edad"))))

fig4_conc <- ggplot(coef_M4a,
                    aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Predictores de la conciencia crítica"
  ) +
  theme_minimal()

## Para ver la figura:
fig4_conc


## ============================================
## Forest plots para Tabla 1 (Modelos M1–M2)
## ============================================


# Resultados de los modelos
coef_tabla1 <- tribble(
  ~modelo, ~predictor,        ~label,              ~b,       ~se,
  # M1a: DV = Afinidad azar
  "M1a",   "A1",              "Tiempo semanal (A1)",   -0.021, 0.026,
  "M1a",   "A5",              "Gasto mensual (A5)",     0.130, 0.014,
  "M1a",   "F1",              "Edad (F1)",             -0.010, 0.010,
  
  # M1b: DV = Gasto azar
  "M1b",   "A1",              "Tiempo semanal (A1)",    0.010, 0.035,
  "M1b",   "A5",              "Gasto mensual (A5)",     0.375, 0.019,
  "M1b",   "F1",              "Edad (F1)",              0.003, 0.013,
  
  # M2a: DV = Patrones temporales
  "M2a",   "A1",              "Tiempo semanal (A1)",    0.129, 0.021,
  "M2a",   "A5",              "Gasto mensual (A5)",     0.021, 0.014,
  "M2a",   "F1",              "Edad (F1)",             -0.005, 0.010,
  "M2a",   "afinidad_azar",   "Afinidad azar",          0.190, 0.027,
  "M2a",   "gasto_azar",      "Gasto azar",             0.243, 0.020,
  "M2a",   "F6",              "Nivel estudios (F6)",    0.017, 0.039,
  "M2a",   "F7",              "Situación laboral (F7)",-0.070, 0.036,
  
  # M2b: DV = Patrones monetarios
  "M2b",   "A1",              "Tiempo semanal (A1)",    0.049, 0.020,
  "M2b",   "A5",              "Gasto mensual (A5)",     0.065, 0.013,
  "M2b",   "F1",              "Edad (F1)",              0.005, 0.009,
  "M2b",   "afinidad_azar",   "Afinidad azar",          0.104, 0.025,
  "M2b",   "gasto_azar",      "Gasto azar",             0.273, 0.019,
  "M2b",   "F6",              "Nivel estudios (F6)",    0.031, 0.036,
  "M2b",   "F7",              "Situación laboral (F7)",-0.074, 0.033
)

# IC 95%
coef_tabla1 <- coef_tabla1 %>%
  mutate(
    ci_low  = b - 1.96 * se,
    ci_high = b + 1.96 * se
  )

# Etiquetas para facets
coef_tabla1 <- coef_tabla1 %>%
  mutate(
    modelo = factor(
      modelo,
      levels = c("M1a", "M1b", "M2a", "M2b"),
      labels = c(
        "M1a: DV = Afinidad azar",
        "M1b: DV = Gasto azar",
        "M2a: DV = Patrones temporales",
        "M2b: DV = Patrones monetarios"
      )
    )
  )

# Orden de predictores en el eje Y (de arriba hacia abajo)
orden_labels <- c(
  "Gasto azar",
  "Afinidad azar",
  "Gasto mensual (A5)",
  "Tiempo semanal (A1)",
  "Nivel estudios (F6)",
  "Situación laboral (F7)",
  "Edad (F1)"
)

coef_tabla1 <- coef_tabla1 %>%
  mutate(
    label = factor(label, levels = rev(orden_labels))
  )

# Forest plot
fig_tabla1 <- ggplot(coef_tabla1,
                     aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(~ modelo) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Modelos M1–M2: exposición gamblificada y patrones oscuros"
  ) +
  theme_minimal()

# Mostrar gráfica
fig_tabla1



## ============================================
## Forest plots para Tabla 2 (Modelos M3–M4)
## ============================================

# Coeficientes y errores estándar según la tabla
coef_tabla2 <- tribble(
  ~modelo, ~predictor,              ~label,                             ~b,       ~se,
  # M3a: DV = Impacto emocional
  "M3a",   "patrones_temporales",   "Patrones temporales",              0.364,    0.037,
  "M3a",   "patrones_monetarios",   "Patrones monetarios",              0.667,    0.042,
  "M3a",   "afinidad_azar",         "Afinidad azar",                   -0.049,    0.025,
  "M3a",   "gasto_azar",            "Gasto azar",                      -0.037,    0.021,
  "M3a",   "A1",                    "Tiempo semanal (A1)",              0.003,    0.021,
  "M3a",   "A5",                    "Gasto mensual (A5)",               0.019,    0.014,
  "M3a",   "F1",                    "Edad (F1)",                        0.000,    0.010,
  
  # M3b: DV = Normalización
  "M3b",   "patrones_temporales",   "Patrones temporales",             -0.027,    0.029,
  "M3b",   "patrones_monetarios",   "Patrones monetarios",              0.016,    0.032,
  "M3b",   "afinidad_azar",         "Afinidad azar",                    1.011,    0.020,
  "M3b",   "gasto_azar",            "Gasto azar",                       0.022,    0.015,
  "M3b",   "A1",                    "Tiempo semanal (A1)",              0.021,    0.016,
  "M3b",   "A5",                    "Gasto mensual (A5)",               0.008,    0.011,
  "M3b",   "F1",                    "Edad (F1)",                       -0.005,    0.008,
  
  # M4a: DV = Conciencia crítica
  "M4a",   "patrones_temporales",   "Patrones temporales",              0.112,    0.038,
  "M4a",   "patrones_monetarios",   "Patrones monetarios",              0.428,    0.046,
  "M4a",   "afinidad_azar",         "Afinidad azar",                   -0.211,    0.030,
  "M4a",   "gasto_azar",            "Gasto azar",                      -0.110,    0.023,
  "M4a",   "A1",                    "Tiempo semanal (A1)",              0.068,    0.023,
  "M4a",   "A5",                    "Gasto mensual (A5)",               0.001,    0.015,
  "M4a",   "F1",                    "Edad (F1)",                       -0.001,    0.011,
  
  # M4b: DV = Impacto emocional (con variables centradas e interacciones)
  "M4b",   "patrones_temporales_c", "Patrones temporales (c)",          0.336,    0.033,
  "M4b",   "patrones_monetarios_c", "Patrones monetarios (c)",          0.596,    0.034,
  "M4b",   "conciencia_critica_c",  "Conciencia crítica (c)",           0.113,    0.033,
  "M4b",   "int_temp_conc",         "Interacción temp×conc",            0.044,    0.043,
  "M4b",   "int_mon_conc",          "Interacción mon×conc",             0.000,    0.042,
  "M4b",   "A1",                    "Tiempo semanal (A1)",              0.000,    0.020,
  "M4b",   "A5",                    "Gasto mensual (A5)",               0.019,    0.014,
  "M4b",   "F1",                    "Edad (F1)",                       -0.001,    0.010
)

# IC del 95 %
coef_tabla2 <- coef_tabla2 %>%
  mutate(
    ci_low  = b - 1.96 * se,
    ci_high = b + 1.96 * se
  )

# Etiquetas para cada modelo
coef_tabla2 <- coef_tabla2 %>%
  mutate(
    modelo = factor(
      modelo,
      levels = c("M3a","M3b","M4a","M4b"),
      labels = c(
        "M3a: DV = Impacto emocional",
        "M3b: DV = Normalización",
        "M4a: DV = Conciencia crítica",
        "M4b: DV = Impacto emocional (+ moderadores)"
      )
    )
  )

# Orden común de predictores en el eje Y
orden_labels <- c(
  "Patrones monetarios (c)",
  "Patrones temporales (c)",
  "Conciencia crítica (c)",
  "Interacción temp×conc",
  "Interacción mon×conc",
  "Patrones monetarios",
  "Patrones temporales",
  "Afinidad azar",
  "Gasto azar",
  "Tiempo semanal (A1)",
  "Gasto mensual (A5)",
  "Nivel estudios (F6)",
  "Situación laboral (F7)",
  "Edad (F1)"
)

coef_tabla2 <- coef_tabla2 %>%
  mutate(
    label = factor(label, levels = rev(orden_labels))
  )

# Forest plot
fig_tabla2 <- ggplot(coef_tabla2,
                     aes(x = b, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(~ modelo) +
  labs(
    x = "Coeficiente (b) con IC 95%",
    y = NULL,
    title = "Modelos M3–M4: consecuencias emocionales y conciencia crítica"
  ) +
  theme_minimal()

# Mostrar gráfica
fig_tabla2

