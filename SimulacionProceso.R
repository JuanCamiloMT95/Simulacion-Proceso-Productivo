# Función

## Librerías
library(tidyverse)
library(truncnorm)

SimulacionProceso <- function(Proceso, Predecesor){
  
  #Identificar actividades sin precedencia:
  dfProceso$iniTiem[!dfProceso$id %in% dfPredecesor$sucesor] <- 0
  
  #Actualizar los tiempos de terminación de la salida
  dfProceso$finTiem <- dfProceso$tiempo + dfProceso$iniTiem
  
  #Actualizar el estado
  dfProceso$estado[!is.na(dfProceso$finTiem)] <- 1
  
  while (sum(is.na(dfProceso$finTiem))>0){
    
    # Se actualiza la informacación de los tiempos de precedencia.
    dfPredecTiemp <-left_join(dfPredecesor, dfProceso %>% 
                                dplyr::select(id,estado,finTiem), 
                              by =c("predecesor" ="id"))
    # Se identifican las presentes que todavía faltan por ser ejecutadas.
    dfFaltantes <- dfPredecTiemp %>% filter(estado == 0)
    
    # Identificar las tareas que se pueden iniciar
    vtHabil <- dfProceso$id[dfProceso$estado == 0 & 
                              !dfProceso$id %in% dfFaltantes$sucesor]
    
    # Identificar el tiempo de inicio de cada actividad
    dfTiempFn <- dfPredecTiemp %>%  filter(estado == 1) %>% group_by(sucesor) %>% 
      summarise(maxTiemp = max(finTiem))
    
    #Actualizar el tiempo de los que ya pueden iniciar
    dfProceso$estado[dfProceso$id %in% vtHabil] <- 1
    
    # Traer el tiempo de incio
    dfProceso <- left_join(dfProceso, dfTiempFn, by = c("id" = "sucesor"))
    
    # Actualizar el tiempo de inicio.
    dfProceso$iniTiem[dfProceso$id %in% vtHabil] <-
      dfProceso$maxTiemp[dfProceso$id %in% vtHabil]
    
    # Actualizar el tiempo final.
    dfProceso$finTiem[dfProceso$id %in% vtHabil] <- 
      dfProceso$iniTiem[dfProceso$id %in% vtHabil] +
      dfProceso$tiempo[dfProceso$id %in% vtHabil]
    
    # Eliminiar la columna de ayuda.
    dfProceso <- dfProceso %>% select(-maxTiemp)
  
    }
  tiempoFinal <- max(dfProceso$finTiem)
  return(tiempoFinal)
  
}
