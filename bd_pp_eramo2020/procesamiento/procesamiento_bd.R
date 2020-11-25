
rm(list = ls())
setwd("/home/rmg/Escritorio/proyecto_maec_programacion/bd_pp_eramo2020/procesamiento/")

##### Cambios en establecimientos1.csv
establecimientos <- read.csv("bd_captura/establecimientos.csv", encoding = "UTF-8")
establecimientos <- establecimientos[-151,-c(13:15,27:30,32,35:39)]
names(establecimientos) <- c("llave","folio_sist","tipo","id_denue","latitud",
                              "longitud","prec","altitud","planeado",
                              "nvo_establec","saludo","acepta","consumo","mercado",
                              "residuos_vc","residuos_vp","residuos_ac","residuos_ap",
                              "residuos_ot","ot_residuos_e","despedida","obs_1",
                              "obs_2","obs_3","obs_4","obs_5")
# Ubica establecimiento con error en coordenadas que se corrige manualmente
subset(establecimientos[,c(1,3:6)], establecimientos$id_denue == 8974)
# Se filtran los casos de entrevistas válidas
establecimientos <- subset(establecimientos, establecimientos$acepta == 1 & 
                             establecimientos$tipo == 2, na.rm = T)


##### Cambios en interview_comments.csv
interview_comments <- read.csv("bd_captura/interview_comments.csv", encoding = "UTF-8")
interview_comments <- interview_comments[,-c(3:5,7,11,13:15)]
names(interview_comments) <- c("llave","folio_sist","estatus","fecha_com","hora_com","autor_com","comentario")


##### Lee tabla lista_residuos.csv
lista_residuos <- read.csv("bd_captura/lista_residuos.csv", encoding = "UTF-8")
names(lista_residuos) <- c("llave","folio_sist","tipo_resid","num_resid")

##### Cambios en lista_tiporesiduos.csv
lista_tiporesiduos <- read.csv("bd_captura/lista_tiporesiduos.csv", encoding = "UTF-8")
lista_tiporesiduos <- lista_tiporesiduos[,-c(6,7,17,18,22)]
lista_tiporesiduos$V4 <- lista_tiporesiduos$V4+1
names(lista_tiporesiduos) <- c("llave","folio_sist","tipo_resid","consec_residuo",
  "nombre_residuo","cantidad_resid","unidad_resid",
  "otra_unidad","cant_r_no_comes","unid_r_no_comes","otra_ur_no_comes",
  "t_colecta","otro_t_colecta","causa_desecho","destino","cant_banco",
  "unidad_banco","otro_destino","autoriza_imag","imagen_resid")

# Se lee tabla de muestra
muestra <- read.csv("bd_operativa/muestra.csv", encoding = "UTF-8")


# Aquí existe un conjunto de procedimientos que
# no pueden reproducirse en R, ya es necesario
# realizar cambios manualmente, que se describen en la bitácora
# de procesamiento, y que tendrían que volverse a aplicar a la tabla
# establecimientos1. 
# Al final, la tabla resultante, denominada igual establecimientos1, 
# se escribe en la carpeta bd_captura.


# Se relee la tabla establecimientos1, ya con los cambios manuales, y 
# se adicionan los campos provenientes de la muestra, y se escribe en la carpeta
# bd_explot.

establecimientos1 <- read.csv("bd_captura/establecimientos1.csv", encoding = "UTF-8")

library(sqldf)

establecimientos2 <- sqldf("select establecimientos1.*, muestra.nombre_establecimiento, muestra.codigo_scian, 
 muestra.personas_ocupadas, muestra.municipio, muestra.tipo_unidad_economica, muestra.sector_scian, 
 muestra.tipo_actividad from establecimientos1 left join muestra on establecimientos1.id_denue == muestra.id_denue")

write.csv(establecimientos2, "bd_explot/establecimientos2.csv")


### Ahora se obtienen las tablas restantes depuradas y complementadas (interview_comments no se modifica):

interview_diagnostics <- read.csv("bd_captura/interview_diagnostics.csv", encoding = "UTF-8")
interview_diagnostics2 <- sqldf("select interview_diagnostics.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica
                                from establecimientos2 join interview_diagnostics
                                 on interview_diagnostics.llave = establecimientos2.llave")
write.csv(interview_diagnostics2, "bd_explot/interview_diagnostics2.csv",fileEncoding = "UTF-8")


lista_residuos2 <- sqldf("select lista_residuos.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica,
                                establecimientos2.sector_scian
                                from lista_residuos join establecimientos2 
                                 on lista_residuos.llave = establecimientos2.llave")
write.csv(lista_residuos2, "bd_explot/lista_residuos2.csv", fileEncoding = "UTF-8")


lista_tiporesiduos2 <- sqldf("select lista_tiporesiduos.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica,
                                establecimientos2.sector_scian
                                from lista_tiporesiduos join establecimientos2 
                                 on lista_tiporesiduos.llave = establecimientos2.llave")
write.csv(lista_tiporesiduos2, "bd_explot/lista_tiporesiduos2.csv", fileEncoding = "UTF-8")




## Promedios
mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0)])
mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0)])
sum(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" & 
                                          (lista_tiporesiduos2$sector_scian == "Secundario" | lista_tiporesiduos2$sector_scian == "Terciario"))])
sum(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & (lista_tiporesiduos2$nombre_establecimiento == "LA HUERTA" |
                                                                               lista_tiporesiduos2$sector_scian == "Primario"))])


mean(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0)])
mean(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])
median(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0)])
sum(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" & 
                                             (lista_tiporesiduos2$sector_scian == "Secundario" | lista_tiporesiduos2$sector_scian == "Terciario"))])
sum(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & (lista_tiporesiduos2$nombre_establecimiento == "LA HUERTA" |
                                                                                     lista_tiporesiduos2$sector_scian == "Primario"))])
