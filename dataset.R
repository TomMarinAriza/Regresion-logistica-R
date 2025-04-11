# ───────────────────── LIBRERÍAS ───────────────────── #
library(tidyverse)
library(broom)

# ─────────────── CARGA Y LIMPIEZA DE DATOS ───────────── #
dates <- read_csv("Dataset/video games sales.csv", show_col_types = FALSE) %>%
  select(-Name, -Rank) %>%               # Eliminar columnas irrelevantes
  distinct() %>%                         # Quitar duplicados
  mutate(
    Year = as.numeric(Year),            # Convertir año a numérico
    Altas_Ventas = ifelse(Global_Sales > 5, 1, 0),  # Crear variable binaria
    Genre = as.factor(Genre),
    Platform = as.factor(Platform),
    Publisher = as.factor(Publisher)
  ) %>%
  drop_na(Year, Genre, Platform, Global_Sales)  # Quitar NA clave

# ─────────────── REGRESIÓN LOGÍSTICA ───────────── #
model <- glm(Altas_Ventas ~ Genre + Platform + Year, 
             data = dates, 
             family = binomial)

# ─────────────── EVALUACIÓN DEL MODELO ───────────── #
# Predicciones
probabilities <- predict(model, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)
real <- dates$Altas_Ventas

# Matriz de confusión y precisión
conf_matrix <- table(Predicho = predictions, Real = real)
accuracy <- mean(predictions == real)

# Mostrar solo resultados relevantes
print(conf_matrix)
cat("Precisión del modelo:", round(accuracy, 3), "\n")

# ─────────────── VISUALIZACIÓN ───────────── #
# Mostrar gráfico en VS Code
# (Requiere que tengas activado un entorno R, como Radian o plot viewer)

tidy(model) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de variables", x = "Variable", y = "Coeficiente")
