# Auditoría del comportamiento del cliente y rediseño de la fidelización

## Módulo 10 · Reto 2 · Grupo I

Integrantes:
- Lourdes Casado Vidigal
- Mónica Pachón Basallo
- Pamela Valembois Madrigal

## Objetivo

Analizar el comportamiento de compra de los clientes y utilizar los
resultados para orientar una propuesta de rediseño del programa de
fidelización.

## Contenido del repositorio

- script_revision_datos.Rmd: código del análisis organizado en chunks.
- README.md: explicación del trabajo e instrucciones de ejecución.
- .gitignore: reglas para excluir datos y archivos privados.
- Archivo .Rproj, si está incluido: configuración del proyecto de RStudio.

## Secuencia del trabajo

1. Importación de los maestros de clientes y tiques.
2. Revisión de dimensiones, tipos de columnas y fechas.
3. Diagnóstico de valores ausentes, duplicados y anomalías.
4. Limpieza documentada, conservando los datos originales.
5. Cruce de clientes y tiques con controles de integridad.
6. Definición y cálculo de los indicadores de compra.
7. Comparación de periodos equivalentes.
8. Segmentación por frecuencia y recurrencia.
9. Estimación del valor observado y escenarios económicos.
10. Interpretación de resultados y propuesta de fidelización.

Los chunks del R Markdown deben ejecutarse desde el principio y en orden.

## Datos necesarios

Los archivos originales no se incluyen en esta subida.
Para reproducir el análisis, deben obtenerse por la vía autorizada
del grupo y colocarse en estas rutas:

- data/Maestro de Clientes.xlsx
- data/Maestro Tiquets Cliente.xlsx

Los nombres y extensiones deben coincidir con las rutas del
chunk de importación.

## Cómo ejecutar el análisis

1. Abrir el proyecto en RStudio o Posit Cloud.
2. Abrir script_revision_datos.Rmd.
3. Colocar los Excel originales en la carpeta data.
4. Comprobar las rutas del chunk de importación.
5. Instalar los paquetes utilizados si faltan.
6. Ejecutar los chunks desde el principio y en orden.
7. Revisar advertencias y controles antes de interpretar los resultados.
8. Generar el informe con Knit cuando la ejecución completa funcione.

El documento debe importar los archivos por sí mismo, sin depender
de tablas cargadas manualmente en el Environment.

## Precauciones y limitaciones

- No subir contraseñas ni tokens de GitHub.
- No subir maestros ni tablas con registros individuales.
- Las operaciones negativas no se interpretan automáticamente
  como devoluciones.
- Las cantidades extremas quedan pendientes de validación.
- El escenario de UPT con filtro de 1 a 100 artículos es provisional.
- No se pueden atribuir causalmente los cambios observados al programa
  de fidelización.
- Los escenarios económicos dependen de supuestos, no de márgenes
  reales facilitados por la empresa.
- Las acciones de fidelización son propuestas que deben evaluarse,
  no resultados ya demostrados.
