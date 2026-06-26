


# Se usa la tabla consolidada para estimar probabilidades de transicion. 
# El tamanio de la poza, es, segun Lancho & Cantu, 2018, ~34,200
# Uso la tabla del SNI consolidada para estimar la proporcion en C, en el anio t, que pasaron a
# N1 en el anio t+1; la proporcion en  N1, el anio t, que pasaroan a 
# N2 el anio t+1, y asi. . 


# la tabla consolidada se lee a la matriz "ecls"
ecls=read.csv("D:\\SNI_Datos\\Combinados_1991_2024_C.csv")
xp=unique(ecls$EXPEDIENTE) # xp es un vector con los numeros de expediente
nx=length(xp)

#
# Niveles es la matriz de numero de transiciones
niveles=matrix(0,ncol=12,nrow=nx)
for (i in 1:nx)
  {
  
  ii=which(ecls$EXPEDIENTE==xp[i]); # ii es el conjunto de indices que hacen referencia a un numero de expediente
  lv=sort(ecls$NIVEL[ii]); # Se ordenan para tener los niveles 1,2,3 C y E
  tlv=table(lv);
  # tlv es una tabla con el numero de veces que el expediente i aparece como de nivel "C", "1" etc.
  # Esto es, por ejemplo, si N1 vale 5, hubieron cuatro transiciones de N1 a N1
  nvs=names(tlv);
  
  # En niveles[,1] estan las transiciones de C a N1 (rc)
  # In niveles[,2] estan las transiciones de C a C (pc)
  tc=which(nvs=="C")
  if(identical(tc,integer(0))) {niveles[i,1]=0;niveles[i,2]=0} else {niveles[i,1]=1;niveles[i,2]=(tlv[[tc]]-1)}
  
  # In niveles[,3] estan las transiciones de N1 a N2 (rN1)
  # In niveles[,4] estan las transiciones de N1 a N1 (pN1)
  tc=which(nvs=="1")
  if(identical(tc,integer(0))) {niveles[i,3]=0;niveles[i,4]=0} else {niveles[i,3]=1;niveles[i,4]=(tlv[[tc]]-1)}
  
  # In niveles[,5] estan las transiciones de N2 a N3
  # In niveles[,6] estan las transiciones de N2 a N2
  tc=which(nvs=="2")
  if(identical(tc,integer(0))) {niveles[i,5]=0;niveles[i,6]=0} else {niveles[i,5]=1;niveles[i,6]=(tlv[[tc]]-1)}
  
  
  # In niveles[,7] estan las transiciones de N3 a E
  # In niveles[,8] estan las transiciones de N3 a N3
  tc=which(nvs=="3")
  if(identical(tc,integer(0))) {niveles[i,7]=0;niveles[i,8]=0} else {niveles[i,7]=1;niveles[i,8]=(tlv[[tc]]-1)}
 
  # In niveles[,9] estan las transiciones de N3 a E
  # In niveles[,10] estan las transiciones de E a E
  tc=which(nvs=="E")
  if(identical(tc,integer(0))) {niveles[i,9]=0} else { niveles[i,9]=1}
  
  # Finalmente, los movimientos de la poza a "C", to "1" and to "2." Supondre que no hay transiciones de la poza al nivel 3 
  tcC=which(nvs=="C")
  tc1=which(nvs=="1")
  tc2=which(nvs=="2")
  if(!identical(tcC,integer(0))) {niveles[i,10]=1}                                                # Poza-> C
  if(!identical(tc1,integer(0)) & length(tc1 > 0)) {niveles[i,11]=1}                              # Poza-> N1
  if(!identical(tc2,integer(0)) & identical(tc1, integer(0)) & length(tc2) > 0) {niveles[i,12]=1} # Poza -> N2
 }
  colnames(niveles)=c("C->N1","C->C","N1->N2","N1->N1","N2->N3","N2->N2","N3->E","N3->N3","E->E","P->C","P->N1","P->N2")
 
  # Ahora cuento el total de transiciones de cada expediente.
  trans=rowSums(niveles)
  #
  tasasM=matrix(0,ncol=12,nrow=nx)
  colnames(tasasM)=c("C->N1","C->C","N1->N2","N1->N1","N2->N3","N2->N2","N3->E","N3->N3","E->E","P->C","P->N1","P->N2")
  for( i in 1:nx){tasasM[i,]=niveles[i,]/trans[i]}
  # Ahora cuento el numero de 0s  por columna, porque el promedio se debe tomar
  # solamente entre valores diferentes de cero
  nz=matrix(0,nrow=1,ncol=12)
  for(j in 1:12){nz[1,j]=length(which(tasasM[,j]>0))}
  
  # El vector de tasas de transicion, finalmente, es:
  tasasV=colSums(tasasM)/nz[1,]
  
  # Y la estructura de ocupaciones en la tabla consolidada es:
  table(ecls$NIVEL)/nrow(ecls)
  
  
  # Figura
  xy=df2[,2]
  plot(xy,10^df2[,3],pch=19,type="b",xlab="Anios",ylab="Personal en el SNII")
  lines(x,z[,1],col="blue")
  lines(x,z[,3],col="red")
  lines(x,z[,2],col="red")
  grid()