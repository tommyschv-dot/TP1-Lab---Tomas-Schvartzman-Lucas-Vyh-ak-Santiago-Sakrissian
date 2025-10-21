# Trabajo Práctico - Laboratorio para el Análisis de Datos Económicos y Financieros

**Integrantes:**  
- Tomás Schvartzman  
- Lucas Vyhñak  
- Santiago Sarkissian  

**Lenguaje:** R  
**Software:** RStudio  
**Universidad:** UTDT  

---

## 📘 Descripción general

Este trabajo práctico integra tres bloques principales:

1. **Análisis de Datos**  
   Se trabaja con datos reales de exportaciones argentinas (septiembre 2025) provenientes del INDEC.  
   El objetivo fue limpiar el dataset, vincularlo con el archivo de países y responder preguntas exploratorias.

2. **Análisis Econométrico**  
   Se utiliza el dataset **Gapminder** para estimar y analizar modelos de regresión, correlaciones y contrastes estadísticos sobre expectativa de vida, ingresos y crecimiento económico.

3. **Simulación**  
   Se simulan distribuciones estadísticas (Chi-cuadrado y Cobb-Douglas) para analizar comportamiento de consumo y efectos de shocks de precios, incluyendo heterogeneidad entre agentes.

---

## 📦 Archivos incluidos

- `TP1_LAB.R` → Contiene **todo el código completo** de los tres ejercicios.  
- `README.md` → Este documento explicativo.

---

## 🧰 Librerías utilizadas

```r
library(readr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
