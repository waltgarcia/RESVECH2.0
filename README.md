# RESVECH 2.0 — Calculadora Clínica de Heridas Crónicas

> Aplicación web interactiva para la valoración y seguimiento de heridas crónicas mediante el índice RESVECH 2.0, adaptado y validado para población mexicana.

🔗 **Aplicación en línea:** [https://waaltergarcia.shinyapps.io/RESVECH2/](https://waaltergarcia.shinyapps.io/RESVECH2/)

---

## ¿Qué es RESVECH 2.0?

RESVECH 2.0 es una escala estandarizada para medir la evolución de las heridas crónicas hacia la cicatrización. Fue desarrollada originalmente en Colombia, adaptada y validada para la población mexicana y de forma general en Latinoamérica, brindando un instrumento cuantitativo, reproducible y de fácil aplicación en la práctica clínica.

La escala considera seis dominios, con una **puntuación total que oscila de 0 a 34 puntos**:

| # | Dominio | Rango |
|---|---------|-------|
| 1 | Dimensiones de la lesión | 0 – 6 |
| 2 | Profundidad / Tejidos afectados | 0 – 4 |
| 3 | Bordes de la herida | 0 – 4 |
| 4 | Tipo de tejido en el lecho | 0 – 4 |
| 5 | Exudado | 0 – 3 |
| 6 | Infección / Inflamación (signos-biopelícula) | 0 – 14 |

### Interpretación del puntaje

| Puntaje | Clasificación | Significado clínico |
|---------|--------------|---------------------|
| 0 – 5 | Cicatrización adecuada | Herida cerca de la resolución |
| 6 – 10 | Retraso en la cicatrización | Revisar plan de cuidados |
| 11 – 15 | Deterioro significativo | Evaluar infección, considerar desbridamiento |
| ≥ 16 | Herida crítica | Evaluación urgente multidisciplinaria |

---

## Funcionalidades de la aplicación

### 📋 Pestaña: Evaluación
- Formulario completo con los 6 dominios del RESVECH 2.0 y los 14 ítems de infección/inflamación.
- Campo opcional para **adjuntar foto clínica de la herida** (JPG/PNG, máx. 5 MB).
- Botones **Limpiar** y **Calcular** al pie del formulario.
- Al calcular, redirige automáticamente a la pestaña de Resultados.

### 📈 Pestaña: Resultados
- Muestra **fecha y hora** exacta de la evaluación.
- Puntaje RESVECH 2.0 con **código de colores** según clasificación clínica.
- Desglose por cada uno de los 6 dominios.
- **Indicaciones clínicas** sugeridas según el puntaje.
- Vista previa de la foto clínica adjunta (si se cargó).
- Opciones de exportación:
  - 📄 **Exportar PDF** — reporte clínico formal, imprimible y listo para anexar al expediente del paciente, con sección de identificación, desglose y recomendaciones.
  - 📝 **Exportar Texto** — resumen en texto plano.
  - 💾 **Guardar en Historial** — agrega el resultado al registro de evolución de la sesión.

### 📊 Pestaña: Histórico
- Gráfica interactiva: **Puntaje RESVECH 2.0 (eje Y) vs. número de evaluación (eje X)**.
- Líneas de referencia para cada umbral de clasificación (verde/amarillo/rojo).
- Tabla de evaluaciones guardadas con fecha/hora y clasificación.
- Botón para limpiar el historial de la sesión.

### 📚 Pestaña: Bibliografía
- Referencias en formato APA de los artículos originales de validación.

### Otras características
- Modo oscuro / claro.
- Interfaz 100 % en español.
- Diseñada para usarse desde cualquier dispositivo con navegador web.

---

## Stack tecnológico

| Componente | Tecnología |
|-----------|-----------|
| Lenguaje | R |
| Framework | Shiny |
| Visualizaciones | plotly |
| Tablas | DT |
| UI | bslib (Bootstrap 4) |
| JS helpers | shinyjs |
| Deploy | ShinyApps.io |

---

## Cómo ejecutar localmente

```r
# 1. Instalar dependencias (solo la primera vez)
install.packages(c("shiny", "shinyjs", "plotly", "DT", "dplyr", "tidyr", "bslib"))

# 2. Ejecutar la aplicación
shiny::runApp("RESVECH2.0 Calculator.R")
```

---

## Referencias bibliográficas

Restrepo-Medrano, J. C., Medina-Pérez, F., & Guzmán-Camargo, S. (2021). Adaptación cultural y validación del índice RESVECH 2.0 en población mexicana. *Revista Iberoamericana de Heridas y Cicatrización, 2*(1), 45-58.

Restrepo-Medrano, J. C., & Verdú Soriano, J. (2011). Desarrollo de un índice de medida de la evolución hacia la cicatrización de las heridas crónicas. *Gerokomos, 22*(4), 176-183.

---

## Contacto

Sugerencias, reportes de error o colaboración académica:

📧 [walter.garciaortiz@gmail.com](mailto:walter.garciaortiz@gmail.com)

---

## Licencia

MIT License — ver archivo [LICENSE](LICENSE) para detalles.
