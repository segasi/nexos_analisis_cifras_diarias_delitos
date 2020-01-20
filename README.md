# ¿Cómo rescatamos la señal?

Este repo contiene los datos, el código y gráficas que usé/generé para el análisis publicado en el número de febrero de 2020 de la revista Nexos.

En este texto discuto como podemos rescatar la "señal" del [reporte diario de homicidios](http://www.informeseguridad.cns.gob.mx/) publicado por la CNS.

Para ello, analizo las similitudes y diferencias en las cifras mensuales de homicidios de acuerdo con los datos del reporte diario y el [reporte mensual de víctimas](https://www.gob.mx/sesnsp/acciones-y-programas/victimas-nueva-metodologia?state=published) del SNSP. 

![My image](https://github.com/segasi/nexos_analisis_cifras_diarias_delitos/blob/master/03_graficas/comparacion_mensual_reporte_vs_victimas_homicidio_doloso_por_edo.png)

Para poder hacer esta comparación primero tuve que construir una base de dato con los datos del reporte diario usando los 242 reportes publicados entre el 3 de abril y el 30 de noviembre de 2019. Después de  descargar los archivos de [www.informeseguridad.cns.gob.mx](http://www.informeseguridad.cns.gob.mx/), extraje y limpié los datos de la tabla incluida en la primera página de cada uno de los mismos, y posteriormente integré los datos una sola base. 

Mientras que el script [`descargar_reportes.R`](https://github.com/segasi/nexos_analisis_cifras_diarias_delitos/blob/master/02_codigo/descargar_reportes.R) contiene el código que usé para descargar los PDFs, el código para construir la base de datos está en los renglones 4 a 212 del script [`analisis_reportes_homicidios.R`](https://github.com/segasi/nexos_analisis_cifras_diarias_delitos/blob/master/02_codigo/analisis_reportes_homicidios.R). Llevé a cabo un proceso de verificación adicional con el código que está en [`verificacion_datos.R`](https://github.com/segasi/nexos_analisis_cifras_diarias_delitos/blob/master/02_codigo/verificacion_datos.R).