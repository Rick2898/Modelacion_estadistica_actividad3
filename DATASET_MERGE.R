d1=read.table("C:\\Users\\Usuario\\Documents\\U_JAVERIANA\\Metodos_simulacion_estadistica\\MODULO_3\\datos_actividad3\\student\\student-mat.csv",sep=";",header=TRUE)
d2=read.table("C:\\Users\\Usuario\\Documents\\U_JAVERIANA\\Metodos_simulacion_estadistica\\MODULO_3\\datos_actividad3\\student\\student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students