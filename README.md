# Pollution reports for schools in Bogotá

Este proyecto utiliza los sensores de polución del proyecto de [Air Filters and Student Learning](https://www.socialscienceregistry.org/trials/9881) para generar reportes de polución para las distintas localidades de Bogotá. 

A cada localidad se le genera un reporte en formato **html** con datos de los úlitmos 30 días desde la fecha de corte de los datos. Utilizamos **rmarkdown** para generar los reportes, apoyados de **ggplot2**, **knitr** y **leaflet** para las visualizaciones

La estructura del proyecto es la siguiente

- `01_Data`
  - `Groundtruth.xlsx` contiene las escuelas del proyecto con sus sensores
  - `Randomization.dta` contiene la asginación a tratamiento
  - `sedes_cor.dta` contiene las coordenadas de las escuelas
  - `Sensors_PM2.5_Long.dta` son los datos de los sensores

- `02_Scripts`
  - `DataReading.R` lee y carga los datos, limpiando algunos detalles
  - `helper_function.R` contiene algunas funciones que se usan para el limpiado
  - `LibraryInstall.R` revisa que los paquetes estén instalados e instala los que no estén
  - `ReportGenerator.R` genera los reportes

- `03_Reports`
  - `Reporte.Rmd` es el **machote** del reporte. Aquí es donde se deben editar textos, el órden de las pestañas/tablas/gráficas, etc. 
  - En esta carpeta se guardan los reportes con extensión .html

- `04_Text`
  - `README.md` contiene la descripción del proyecto
  
## ¿Cómo editar los reportes?

Para editar los reportes, es necesario editar el machote `Reporte.Rmd` o el generador `ReportGenerator.R` en los siguientes casos

| Cambio | Archivo a editar |
| ------ | ---------------- |
| Cambiar un texto | Report.Rmd |
| Agregar una gráfica o tabla | Ambos|
| Cambiar un elemento de una gráfica (título, colores, etc.)| ReportGenerator.R |
| Cambiar orden del reporte (pestañas, textos, etc.) | Report.Rmd |
| Cambiar nombre o formato de los outputs | ReportGenerator.R |



## ¿Cómo generar los reportes?

Para generar los reportes, es necesario realizar algunos pasos previos tras descargar/abrir el proyecto completo. Estos son, en orden:

1. Correr `LibraryInstall.R` para asegurarse de tener todos los paquetes necesarios instalados

2. Revisar que en la carpeta `01_Data`  estén presentes los archivos de **Groundtruth**, **Randomization**, **Coordenadas de las sedes** y la carpeta con los **Shapefiles**

3. Revisar que en la carpeta `01_Data` esté presente la última versión de los datos de sensores (esta será la que usará el script que genera el reporte). Revisar que su nombre coincida con el nombre en el script de `DataReading.R`. 

4. Revisar que el archivo `Reporte.Rmd` esté en la carpeta de Reportes

5. Correr `ReportGenerator.R`. Los archivos se generaran a la carpeta `03_Reports` con extensión html. El nombre del archivo será el nombre de la localidad. Este proceso reescribe cualquier otro reporte con el mismo nombre que ya estuviera presente en la carpeta (e.g. reportes de la iteración pasada)

## Problemas comunes al generar los reportes

1. **Error al correr ReportGenerator**: Revisar que las libererías estén instalados, que todos los datos estén donde deben estar y que los paths sean correctos. Si se sigue rompiendo, probablemente es una cuestión de los datos (nuevo formato, nuevo número de mediciones por día, cambio en el nombre de las localidades, etc.)

2. **El reporte se genera pero no se ve las tablas/gráficas**: Revisar la gráfica adecuada en `ReportGenerator.R`, probablemente se debe a un tema en los datos

3. **Las gráficas del reporte tienen patrones extraños**: Probably missing data, o un cambio en los datos (formato de las fechas, nombres de las localidades, etc.)

4. **Las cuentas/porcentajes/probabilidades del uso de sensores y filtros da valores extraño (2000% de lecturas, probabilidad de uso mayor a 1, etc.)**: Probablemente hubo un cambio en los datos (traen más o menos mediciones) y hay que ajustar el código en `ReportGenerator.R``


**Por cualquier otro issue, pregunta o duda contactar a joseulisesquevedo\@gmail.com**