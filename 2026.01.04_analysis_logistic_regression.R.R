#Cargo tidyveverse
library (tidyverse)

# 1. CARGAR DATOS
datos <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# 2. VISTAZO GENEREAL
print(paste("Número de pacientes:",nrow(datos)))
print(paste("Número de variables:",ncol(datos)))

# 3. ANÁLISIS BMI
Print("---Resumen Estadístico del BMI---")
summary(datos$BMI)
plot(datos$BMI)

## Valores extremos
pacientes_bmi_extremo<- datos %>%
  filter(BMI>50) %>%
  nrow()
print(paste("Pacientes con BMI>50:", pacientes_bmi_extremo))

# 4. LIMPIEZA DE DATOS
#Excluir BMIs extremos (>80)
datos_limpios <- datos %>%
  filter(BMI<80)

#Comprobamos cúantos pacientes hemos eliminado
pacientes_eliminados<- nrow(datos) - nrow(datos_limpios)
print(paste("Pacientes eliminados por criterio clínico:", pacientes_eliminados))
print("Nuevo maximo de BMI:")
max(datos_limpios$BMI)

# 5. VISUALIZACIÓN
ggplot(data=datos_limpios, aes(x =BMI)) + #Datos que uso, eje X
  geom_histogram (binwidth = 1, #Tipo de gráfico
                  fill = "steelblue", #Color de relleno
                  color = "white",
                  alpha = 0.8)+ #Transparencia
  geom_vline(aes(xintercept =25), color = "orange",linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 30), color = "red", linetype = "dashed", size = 1)+
  
  scale_x_continuous(breaks = seq(10,80, by = 5))+
  
  labs(title ="Distribución del BMI en la población estudiada",
       subtitle = "Datos filtrados (BMI < 80)",
       x = "Índice de Masa Corporal (BMI)",
       y = "Número de Pacientes") +
  theme_minimal()

# 6. COMPARATIVA: ¿Pesan más los diabéticos?

# Paso A: Etiquetar los datos para que el gráfico se entienda
# La variable 'Diabetes_binary' suele ser 0 (Sano) y 1 (Diabético)
datos_comparativos <- datos_limpios %>% 
  mutate(Estado = factor(Diabetes_binary, 
                         levels = c(0, 1), 
                         labels = c("Sin Diabetes", "Con Diabetes")))

# Paso B: El Boxplot (La herramienta estándar para comparar grupos)
ggplot(data = datos_comparativos, aes(x = Estado, y = BMI, fill = Estado)) +
  geom_boxplot(alpha = 0.7) +
  
  # Añadimos la línea de obesidad para referencia
  geom_hline(yintercept = 30, color = "red", linetype = "dashed") +
  
  labs(title = "Impacto del BMI en el desarrollo de Diabetes",
       subtitle = "Comparativa de medianas entre grupos",
       x = "Condición del Paciente",
       y = "Índice de Masa Corporal (BMI)") +
  theme_minimal() +
  theme(legend.position = "none") # Quitamos la leyenda porque ya está claro en el eje X

# Paso C: Los números exactos (Para confirmar lo que ven tus ojos)
resumen_grupos <- datos_comparativos %>%
  group_by(Estado) %>%
  summarise(
    Media_BMI = mean(BMI),
    Mediana_BMI = median(BMI),
    Total_Pacientes = n()
  )

print(resumen_grupos)

# 7. MODELADO: Cuantificación del Riesgo (Regresión Logística)
modelo_riesgo<- glm(Diabetes_binary~BMI,
                    data=datos_limpios,
                    family = "binomial")
summary(modelo_riesgo)
odds_ratios<- exp(coef(modelo_riesgo))
print(odds_ratios)
#Odds_ratio = 1.09 -> Por cada punto BMI el riesgo de diabetes se multiplica por 1.09, aumenta un 9%.

# 8. VISUALIZACIÓN DEL MODELO: Curva de Riesgo
ggplot(datos_limpios, aes ( x = BMI, y = Diabetes_binary))+
  geom_point(alpha = 0.05, position = position_jitter(height = 0.05))+
  stat_smooth(method = "glm",
              method.args = list(family ="binomial"),
              color = "red",
              size = 1.5)
  theme_minimal()

?glm()
# 9. REFINANDO EL MODELO: Categorías vs Números
  
  # Paso A: Crear las categorías de la OMS
  datos_con_categorias <- datos_limpios %>%
    mutate(Categoria_BMI = case_when(
      BMI < 25 ~ "1. Normal",
      BMI >= 25 & BMI < 30 ~ "2. Sobrepeso",
      BMI >= 30 & BMI < 35 ~ "3. Obesidad I",
      BMI >= 35 & BMI < 40 ~ "4. Obesidad II",
      BMI >= 40 ~ "5. Obesidad III (Mórbida)"
    ))
  
  # Paso B: Entrenar el modelo usando la CATEGORÍA, no el número
  modelo_categorias <- glm(Diabetes_binary ~ Categoria_BMI, 
                           data = datos_con_categorias, 
                           family = "binomial")
  
  # Paso C: Ver los Odds Ratios por grupo
  exp(coef(modelo_categorias))
  