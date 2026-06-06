# grafico1

Carpeta portable para reproducir el Gráfico 1 del artículo final.

## Contenido

- `scripts/01_prepare_data.R`: limpieza y transformación explícita de los insumos.
- `scripts/02_render_grafico1.R`: construcción y exportación del gráfico.
- `data/raw/`: copia de los insumos crudos relevantes en formato `.rds`.
- `data/processed/`: insumo procesado listo para graficar en formato `.rds`.
- `output/`: exportaciones del gráfico (`.png` y `.svg`).

## Cómo usar

Desde esta carpeta, correr:

```r
Rscript scripts/01_prepare_data.R
Rscript scripts/02_render_grafico1.R
```

## Notas

- Esta carpeta no depende de archivos del resto del repositorio.
- Los `.rds` de `data/raw/` son copias de los insumos originales usados para este gráfico:
  - tabulados de ingresos,
  - tabulados de gastos,
  - tabulado de promedios nacionales,
  - mapeo de categorías de gasto.
- El archivo `data/processed/grafico1_processed.rds` se genera desde `scripts/01_prepare_data.R`.
