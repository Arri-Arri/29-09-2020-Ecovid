##Hogares mexicanos más allá del confinamiento: Un análisis de desempleo y subocupación en tiempos de pandemia##
##Ali M. Arrieta##
##28-08-2020##


rm(list=ls())
gc()
setwd("~/Trabajo&Covid")

Ecovid <- read.csv("ECOVID/ecovid0420.csv", header=TRUE)

library(tidyverse)
library(car) 
#install.packages('fdth')

summary(Ecovid$CLASE2)
Ecovid <- Ecovid %>% 
  mutate(
    PNEA_disp=case_when(
      CLASE2==3 ~ 1,
      CLASE2==1 ~ 0,
      CLASE2==2 ~ 0,
      CLASE2==4 ~ 0))
  
table(Ecovid$PNEA_disp)

##Variable dependiente##
summary(Ecovid$PC6)
Ecovid <- Ecovid %>% 
  mutate(
    IncluML_disp=case_when(
      PC6==1|PC6==2|PC6==3|PC6==4 ~1,
      PC6==5|PC6==6~2,
      PD1==2~3),
      IncluML_disp=factor(IncluML_disp, levels = c("1", "2","3"), labels = c("Regresa", "No regresa", "Disponible")
        ))

table(Ecovid$IncluML_disp)

Ecovid <- Ecovid %>% 
  mutate(    
    IncluMLB_disp=case_when(
    PC6==1|PC6==2|PC6==3|PC6==4 ~1,
    PC6==5|PC6==6~0,
    PD1==2~0)) 
   
table(Ecovid$IncluML_disp)
table(Ecovid$IncluMLB_disp)

Disponibles <- filter(Ecovid, IncluML_disp %in% c("Regresa", "No regresa", "Disponible"), PNEA_disp=="1")
table(Disponibles$IncluML_disp)
table(Disponibles$IncluMLB_disp)

##Variable independientes##

##Edad##
a <- fdt(Disponibles$PB2, start=18, end=98, h=1)

Disponibles <- Disponibles  %>% 
  mutate(
    Edad_disp=PB2,
    Edad_disp= recode(Edad_disp, "99=NA"),
    Group_edad_disp= case_when(
    Edad_disp <30 ~ 1,
    Edad_disp >=30 & Edad_disp <40 ~ 2,
    Edad_disp >=40 & Edad_disp <50 ~ 3,
    Edad_disp >=50 & Edad_disp <99 ~ 4),
    Group_edad_disp=factor(Group_edad_disp, levels = c("1", "2","3","4"), labels = c("18-29", "30-39", "40-49","50+")
        ) )
  
summary(Disponibles$Edad_disp)
##sumedad <- summarySE(Disponibles, measurevar="Edad_disp", groupvars=c("PNEA_disp"), na.rm=T)##
table(Disponibles$Group_edad_disp)

table(Disponibles$Group_edad_disp)
table1 <- summary(Disponibles$Group_edad_disp)
tabla1 <- as.data.frame(table(grupos=Disponibles$Group_edad_disp))
b <- transform(tabla1,
          Acum=cumsum(Freq),
          Prop=round(prop.table(Freq),2),
          Prop.Acum=round(cumsum(prop.table(Freq)),2))

##Sexo y educacion##
summary(Disponibles$PB1)
summary(Disponibles$PB3)

Disponibles <- Disponibles  %>% 
  mutate(
    Sexo_disp= case_when(
      PB1==1~0,
      PB1==2~1),
    Escolaridad_disp=case_when(
      PB3==0|PB3==1|PB3==2 ~1,
      PB3==3~2,
      PB3==4|PB3==5|PB3==6|PB3==7~3,
      PB3==8|PB3==9~4),
Sexo_disp=factor(Sexo_disp, levels = c("0","1"), labels = c("Hombre", "Mujer")),
Escolaridad_disp=factor(Escolaridad_disp, levels =c("1","2","3","4") , labels = c("Primaria", "Secundaria", "Prepa-Bachill", "Lic.Maest.Doc"))
) 


table(Disponibles$Sexo_disp)
table(Disponibles$Escolaridad_disp)
tabla2 <- as.data.frame(table(grupos=Disponibles$Escolaridad_disp))

##Estructura y composicion del hogar##
summary(Disponibles$PA1)
summary(Disponibles$PA2_1_1)
summary(Disponibles$PA2_2_1)
Disponibles <- Disponibles  %>% 
  mutate(
    Num_Homb_disp=PA2_1_1,
    Num_Muj_disp=PA2_2_1,
    Num_Homb_disp= recode(Num_Homb_disp, "NA=0"),
    Num_Muj_disp= recode(Num_Muj_disp, "NA=0"),
    Num_Total_disp= (Num_Homb_disp+Num_Muj_disp),
    Num_Total1_disp=case_when(
    Num_Total_disp==1|Num_Total_disp==2|Num_Total_disp==3~1,
    Num_Total_disp==4~2,
    Num_Total_disp>=5~3)
    )
     
table(Disponibles$Num_Total_disp)
table(Disponibles$Num_Total1_disp)
summary(Disponibles$Num_Total_disp)

##Composicion##
Aux1 <- Disponibles
Aux2<-   Aux1 %>%
  pivot_longer(cols = c(`PA2_H_1`,`PA2_H_2`,`PA2_H_3`,`PA2_H_4`,`PA2_H_5`,`PA2_H_6`,`PA2_H_7`,`PA2_H_8`,`PA2_H_9`), names_to = "Hombres", values_to = "Edad_H") %>%
  mutate(
  H65_disp= case_when(
    Edad_H>65 ~1,
    Edad_H<=65~0),
  H15_disp= case_when(
    Edad_H<15 ~1,
    Edad_H>=15~0),
  H15_65_disp= case_when(
    Edad_H>=15 & Edad_H<=65~1,
    Edad_H<15 & Edad_H>65  ~0),
  H15_disp=recode(H15_disp, "1=1; NA=0"),
  H65_disp=recode(H65_disp, "1=1; NA=0"),
  H15_65_disp=recode(H15_65_disp, "1=1; NA=0")
   )

Aux3<-   Aux1 %>%  
  pivot_longer(cols = c(`PA2_M_1`,`PA2_M_2`,`PA2_M_3`,`PA2_M_4`,`PA2_M_5`,`PA2_M_6`,`PA2_M_7`,`PA2_M_8`,`PA2_M_9`), names_to = "Mujeres", values_to = "Edad_M") %>%
   mutate(
    M65_disp= case_when(
      Edad_M>65 ~1,
      Edad_M<=65~0),
    M15_disp= case_when(
      Edad_M<15 ~1,
      Edad_M>=15~0),
    M15_65_disp= case_when(
      Edad_M>=15 & Edad_M<=65~1,
      Edad_M<15 & Edad_M>65  ~0),
    M15_disp=recode(M15_disp, "1=1; NA=0"),
    M65_disp=recode(M65_disp, "1=1; NA=0"),
    M15_65_disp=recode(M15_65_disp, "1=1; NA=0")
    )
   
   
Hombres_1565  <- Aux2 %>% 
  count (Id, H15_65_disp , sort  =  TRUE , name="Hombres_1565_disp")
Hombres_1565  <-   filter(Hombres_1565, H15_65_disp %in% c("1"))  

Hombres_15  <- Aux2 %>% 
  count (Id,H15_disp , sort  =  TRUE , name  =  "Hombres_15_disp" )
Hombres_15  <-   filter(Hombres_15, H15_disp %in% c("1"))  

Hombres_65  <- Aux2 %>% 
  count (Id,H65_disp , sort  =  TRUE , name  =  "Hombres_65_disp" )
Hombres_65  <-   filter(Hombres_65, H65_disp %in% c("1"))  


Mujeres_1565  <- Aux3 %>% 
  count (Id, M15_65_disp , sort  =  TRUE , name  =  "Mujeres_1565_disp")
Mujeres_1565  <-   filter(Mujeres_1565, M15_65_disp %in% c("1"))  

Mujeres_15  <- Aux3 %>% 
  count (Id,M15_disp , sort  =  TRUE , name  =  "Mujeres_15_disp" )
Mujeres_15  <-   filter(Mujeres_15, M15_disp %in% c("1"))  

Mujeres_65  <- Aux3 %>% 
  count (Id,M65_disp , sort  =  TRUE , name  =  "Mujeres_65_disp" )
Mujeres_65  <-   filter(Mujeres_65, M65_disp %in% c("1"))  


Disponibles <- merge(Disponibles,Hombres_1565, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Hombres_15, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Hombres_65, by = c("Id"), all.x=TRUE)

Disponibles <- merge(Disponibles,Mujeres_1565, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Mujeres_15, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Mujeres_65, by = c("Id"), all.x=TRUE)

Disponibles <- Disponibles %>%
               mutate(
                 Hombres_1565_disp=recode(Hombres_1565_disp, "NA=0"),
                 Hombres_15_disp=recode(Hombres_15_disp, "NA=0"),
                 Hombres_65_disp=recode(Hombres_65_disp, "NA=0"),
                 Mujeres_1565_disp=recode(Mujeres_1565_disp, "NA=0"),
                 Mujeres_15_disp=recode(Mujeres_15_disp, "NA=0"),
                 Mujeres_65_disp=recode(Mujeres_65_disp, "NA=0"),
                 Suma_H=(Hombres_1565_disp+Hombres_15_disp+Hombres_65_disp),
                 Suma_M=(Mujeres_1565_disp+Mujeres_15_disp+Mujeres_65_disp),
                 Prueba_H=(Num_Homb_disp-Suma_H),
                 Prueba_M=(Num_Muj_disp-Suma_M)
                  )
str(Disponibles$Prueba_H)
str(Disponibles$Prueba_M)

Disponibles<- select(Disponibles, -H15_65_disp,-H15_disp,-H65_disp)
Disponibles<- select(Disponibles, -M15_65_disp,-M15_disp,-M65_disp)

Disponibles <- rename(Disponibles, Id_disp = Id, PER_disp = PER, Entidad_disp = ENT)

##Indicadores de estructura y composicion##  

library(dplyr) 
  
  Disponibles  %>% 
  select(Hombres_15_disp, Hombres_65_disp, Mujeres_15_disp, Mujeres_65_disp) %>% 
  rowSums(na.rm=TRUE) -> Disponibles$Pob15y65_disp 
    
  Disponibles  %>% 
  select(Hombres_1565_disp, Mujeres_1565_disp) %>%
  rowSums(na.rm=TRUE) -> Disponibles$Pobprod_disp 
   
   
   Disponibles <- Disponibles%>%
                  mutate(IDD_disp=(Pob15y65_disp/Pobprod_disp),
                         RM_disp=(Num_Homb_disp/Num_Muj_disp),
                         IDD_disp=recode(IDD_disp, "Inf=10"),
                         IDD1_disp=case_when(
                           IDD_disp<=0.25~1,
                           IDD_disp>0.25 & IDD_disp<=0.5~2,
                           IDD_disp>0.5~3),
                         RM1_disp=case_when(
                           RM_disp<1~1,
                           RM_disp==1~2,
                           RM_disp>1~3)
                         )
                         
                         
summary(Disponibles$IDD_disp)
table(Disponibles$IDD1_disp)
table(Disponibles$RM_disp)
table(Disponibles$RM1_disp)

##Otras variables independientes##
summary(Disponibles$PA3_1)
table(Disponibles$PA3_1)
summary(Disponibles$PA3_1_1)
table(Disponibles$PA3_1_1)

summary(Disponibles$PA3_2)
table(Disponibles$PA3_2)
summary(Disponibles$PA3_2_1)
table(Disponibles$PA3_2_1)

summary(Disponibles$PA3_3)
table(Disponibles$PA3_3)
summary(Disponibles$PA3_3_1)
table(Disponibles$PA3_3_1)

table(Disponibles$PC1_1)            
table(Disponibles$PC1_2)   
table(Disponibles$PC1_3) 
table(Disponibles$PC1_4)   
table(Disponibles$Tipo_empleo_disp) 
table(Disponibles$PC4)  
table(Disponibles$Razon_notrabajo_disp)
table(Disponibles$PC5_1)
table(Disponibles$PC5_2)
table(Disponibles$PC5_3)

table(Disponibles$PE3)
table(Disponibles$PE5)
table(Disponibles$PE5_1)
table(Disponibles$PE10)
table(Disponibles$PE14_1)
table(Disponibles$PE14_2)
table(Disponibles$PE14_3)
table(Disponibles$PE14_4)
table(Disponibles$PE17)
table(Disponibles$PF15_1) 

table(Disponibles$PG1) 
table(Disponibles$PG2) 
table(Disponibles$PG3) 

table(Disponibles$PG4_4) 
table(Disponibles$Num_Total_disp) 

Disponibles <- Disponibles %>%
  mutate(Num_trabajan_disp=PA3_1_1,
         Num_queaseres_disp=PA3_2_1,
         Num_estudiantes_disp=PA3_3_1, 
         Prop_trabajan_disp=(Num_trabajan_disp/Num_Total_disp),
         IDE_disp=case_when(
           Prop_trabajan_disp<=0.25~1,
           Prop_trabajan_disp<=0.50~2,
           Prop_trabajan_disp<=1 ~3),
         PA4_1=recode(PA4_1, "NA=99"),
         PA4_2=recode(PA4_2, "NA=99"),
         PA4_3=recode(PA4_3, "NA=99"),
         PA4_4=recode(PA4_4, "NA=99"),
         Tecnologia_disp=case_when(
           PA4_1==99 & PA4_2==99 & PA4_3==99 & PA4_4==99~0,
           PA4_1==1~1,
           PA4_2==2~1,
           PA4_3==3~1,
           PA4_4==4~1),
         Tipo_empleo_disp=case_when(
           PC1_1==1 ~1,
           PC1_2==2~2,
           PC1_3==3~3,
           PC1_4==4 & PD3==1 & PC1_1=="NA" ~ 4),
         Razon_notrabajo_disp=case_when(
           PC4==1~1,
           PC4==3~2,
           PC4==2~3,
           PC4>3~3),
           Num_Total1_disp=Num_Total_disp,
           Num_Total1_disp=recode(Num_Total1_disp, "12=11"),
           Num_Total1_disp=recode(Num_Total1_disp, "13=11"),
           Num_Total1_disp=recode(Num_Total1_disp, "14=11")
  )

table(Disponibles$Num_Total1_disp) 

table(Disponibles$PC1_1) 
table(Disponibles$Prop_trabajan_disp) 
table(Disponibles$IDE_disp ) 
table(Disponibles$Tipo_empleo_disp ) 
summary(Disponibles$Prop_trabajan_disp)             
table(Disponibles$Tecnologia_disp )             
table(Disponibles$Razon_notrabajo_disp)

Disponibles <- Disponibles %>%
  select(ends_with("_disp"))

##Distribucion de las variables de la muestra por categorias##

table(Disponibles$Sexo_disp, Disponibles$IncluML_disp)
table(Disponibles$Group_edad_disp, Disponibles$IncluML_disp)
table(Disponibles$Escolaridad_disp, Disponibles$IncluML_disp)
table(Disponibles$Num_Total1_disp , Disponibles$IncluML_disp)
table(Disponibles$Tipo_empleo_disp , Disponibles$IncluML_disp)
table(Disponibles$IDD1_disp , Disponibles$IncluML_disp)
table(Disponibles$RM1_disp , Disponibles$IncluML_disp)
table(Disponibles$Tecnologia_disp  , Disponibles$IncluML_disp)
table(Disponibles$Razon_notrabajo_disp  , Disponibles$IncluML_disp)
table(Disponibles$IDE_disp , Disponibles$IncluML_disp)

table(Disponibles$Sexo_disp , Disponibles$PA3_2)
table(Disponibles$Sexo_disp , Disponibles$PA3_1)

table(Disponibles$PB3 )
table(Disponibles$Escolaridad_disp)
table(Disponibles$Años_escol_disp)
table(Disponibles$exp_disp )
table(Disponibles$exp2_disp )
Disponibles <- Disponibles %>%
  mutate(Años_escol_disp=case_when(
    PB3 =="0"~0,
    PB3 =="1"~2,
    PB3 =="2"~7,
    PB3 =="3"~11,
    PB3 =="4"~13,
    PB3 =="5"~13,
    PB3 =="6"~15,
    PB3 =="7"~13,
    PB3 =="8"~17,
    PB3 =="9"~19),
    exp_disp=(Edad_disp-Años_escol_disp-6),
    exp_disp=recode(exp_disp, "-5=0"),
    exp_disp=recode(exp_disp, "-4=0"),
    exp_disp=recode(exp_disp, "-3=0"),
    exp_disp=recode(exp_disp, "-2=0"),
    exp_disp=recode(exp_disp, "-1=0"),
    exp2_disp=(exp_disp*exp_disp)
    
  )



##library(car)##
##tabla1 <-xtabs(FAC_PER~Sexo_disp + IncluML_disp, data = Disponibles)##
##tabla_matp <-xtabs(FACTOR07~P102+DOMINIO+urbano, data = modulo1)##

##Creacion de variables dummy##

##install.packages("dummies")##
library(dummies)


Disponibles <- cbind(Disponibles, dummy(Disponibles$Sexo_disp, sep = "_"))
names(Disponibles) <- gsub("Disponibles", "Sexo", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$Group_edad_disp , sep = "_"))
names(Disponibles) <- gsub("Disponibles", "Edad", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$Escolaridad_disp , sep = "_"))
names(Disponibles) <- gsub("Disponibles", "Escol", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$Num_Total1_disp , sep = "_"))
names(Disponibles) <- gsub("Disponibles", "Tamaño", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$IDD1_disp, sep = "_"))
names(Disponibles) <- gsub("Disponibles", "IDD", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$RM1_disp  , sep = "_"))
names(Disponibles) <- gsub("Disponibles", "RM", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$Tecnologia_disp  , sep = "_"))
names(Disponibles) <- gsub("Disponibles", "Tec", names(Disponibles))

Disponibles <- cbind(Disponibles, dummy(Disponibles$IDE_disp, sep = "_"))
names(Disponibles) <- gsub("Disponibles", "IDE", names(Disponibles))


##Modelos bivariados##

library(nnet)
Bi1 <- multinom(IncluML_disp~ Sexo_Mujer, data = Disponibles, trace=FALSE)
summary(Bi1, cor=FALSE, wald=TRUE)
z1 <- summary(Bi1)$coefficients/summary(Bi1)$standard.errors
p1 <- (1-pnorm(abs(z1),0,1))*2
coefBi1 <- coef(Bi1)
exp(coefBi1)

Bi2 <- multinom(IncluML_disp~ `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` , data = Disponibles)
summary(Bi2, cor=FALSE, wald=TRUE)
z2 <- summary(Bi2)$coefficients/summary(Bi2)$standard.errors
p2 <- (1-pnorm(abs(z2),0,1))*2
coefBi2 <- coef(Bi2)
exp(coefBi2)

Bi3 <- multinom(IncluML_disp~ `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc` , data = Disponibles)
summary(Bi3, cor=FALSE, wald=TRUE)
z3 <- summary(Bi3)$coefficients/summary(Bi3)$standard.errors
p3 <- (1-pnorm(abs(z3),0,1))*2
coefBi3 <- coef(Bi3)
exp(coefBi3)

Bi4 <- multinom(IncluML_disp~ `Tamaño_1` +`Tamaño_2` , data = Disponibles)
summary(Bi4, cor=FALSE, wald=TRUE)
z4 <- summary(Bi4)$coefficients/summary(Bi4)$standard.errors
p4 <- (1-pnorm(abs(z4),0,1))*2
coefBi4 <- coef(Bi4)
exp(coefBi4)

Bi5 <- multinom(IncluML_disp~ `IDD_1` +`IDD_2` , data = Disponibles)
summary(Bi5, cor=FALSE, wald=TRUE)
z5 <- summary(Bi5)$coefficients/summary(Bi5)$standard.errors
p5 <- (1-pnorm(abs(z5),0,1))*2
coefBi5 <- coef(Bi5)
exp(coefBi5)


Bi6 <- multinom(IncluML_disp~ `IDE_1` +`IDE_2` , data = Disponibles)
summary(Bi6, cor=FALSE, wald=TRUE)
z6 <- summary(Bi6)$coefficients/summary(Bi6)$standard.errors
p6 <- (1-pnorm(abs(z6),0,1))*2
coefBi6 <- coef(Bi6)
exp(coefBi6)

Bi7 <- multinom(IncluML_disp~ `RM_1` +`RM_2` , data = Disponibles)
summary(Bi7, cor=FALSE, wald=TRUE)
z7 <- summary(Bi7)$coefficients/summary(Bi7)$standard.errors
p7 <- (1-pnorm(abs(z7),0,1))*2
coefBi7 <- coef(Bi7)
exp(coefBi7)

Bi8 <- multinom(IncluML_disp~ `Tec_1` , data = Disponibles)
summary(Bi8, cor=FALSE, wald=TRUE)
z8 <- summary(Bi8)$coefficients/summary(Bi8)$standard.errors
p8 <- (1-pnorm(abs(z8),0,1))*2
coefBi8 <- coef(Bi8)
exp(coefBi8)

##Cuantitativas##

Bi9 <- multinom(IncluML_disp~ `Num_Total_disp`, data = Disponibles)
summary(Bi9, cor=FALSE, wald=TRUE)
z9 <- summary(Bi9)$coefficients/summary(Bi9)$standard.errors
p9 <- (1-pnorm(abs(z9),0,1))*2
coefBi9 <- coef(Bi9)
exp(coefBi9)

Bi10 <- multinom(IncluML_disp ~ `Edad_disp`, data = Disponibles)
summary(Bi10, cor=FALSE, wald=TRUE)
z10 <- summary(Bi10)$coefficients/summary(Bi10)$standard.errors
p10 <- (1-pnorm(abs(z10),0,1))*2
coefBi10 <- coef(Bi10)
exp(coefBi10)

Bi11 <- multinom(IncluML_disp ~ `exp_disp`, data = Disponibles)
summary(Bi11, cor=FALSE, wald=TRUE)
z11 <- summary(Bi11)$coefficients/summary(Bi11)$standard.errors
p11 <- (1-pnorm(abs(z11),0,1))*2
coefBi11 <- coef(Bi11)
exp(coefBi11)

Bi12 <- multinom(IncluML_disp ~ `exp2_disp`, data = Disponibles)
summary(Bi12, cor=FALSE, wald=TRUE)
z12 <- summary(Bi12)$coefficients/summary(Bi12)$standard.errors
p12 <- (1-pnorm(abs(z12),0,1))*2
coefBi12 <- coef(Bi12)
exp(coefBi12)


##Modelo completo - metodo Spwise##

Mult0 <- multinom(IncluML_disp~ 1, data = Disponibles, trace= FALSE)
summary(Mult0, cor=FALSE, Wald=TRUE)

Mult0_1 <- update(Bi10, IncluML_disp ~ 1 , data=na.omit(Disponibles[ , all.vars(formula(Bi10))]))
summary(Mult0_1, cor=FALSE, Wald=TRUE)

anova(Mult0, Bi1)
anova(Mult0, Bi2)
anova(Mult0, Bi3)
anova(Mult0, Bi4)
anova(Mult0, Bi5)
anova(Mult0, Bi6)
anova(Mult0, Bi7)
anova(Mult0, Bi8)
anova(Mult0, Bi9)
anova(Mult0_1, Bi10)
anova(Mult0_1, Bi11)
anova(Mult0_1, Bi12)


##Comparacion 2 Ronda##

Mult1 <- multinom(IncluML_disp~ Sexo_Mujer, data = Disponibles, trace= FALSE) ##Mejor modelo- Menor varianza##
summary(Mult1, cor=FALSE, Wald=TRUE)

Mult2 <- multinom(IncluML_disp~ Sexo_Mujer+`Edad_18-29` +`Edad_30-39`+ `Edad_40-49`, data = Disponibles, trace= FALSE)
summary(Mult2, cor=FALSE, Wald=TRUE)

Mult3 <- multinom(IncluML_disp~ Sexo_Mujer+ `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc` , data = Disponibles, trace= FALSE)
summary(Mult3, cor=FALSE, Wald=TRUE)

Mult4 <- multinom(IncluML_disp~ Sexo_Mujer+ `Tamaño_1` +`Tamaño_2` , data = Disponibles, trace= FALSE)
summary(Mult4, cor=FALSE, Wald=TRUE)

Mult5 <- multinom(IncluML_disp~ Sexo_Mujer+ `IDD_1` +`IDD_2` , data = Disponibles, trace= FALSE)
summary(Mult5, cor=FALSE, Wald=TRUE)

Mult6 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` , data = Disponibles, trace= FALSE)
summary(Mult6, cor=FALSE, Wald=TRUE)

Mult7 <- multinom(IncluML_disp ~ Sexo_Mujer + `RM_1` +`RM_2` , data = Disponibles, trace= FALSE)
summary(Mult7, cor=FALSE, Wald=TRUE)

Mult8 <- multinom(IncluML_disp ~ Sexo_Mujer + `Tec_1` , data = Disponibles, trace= FALSE)
summary(Mult8, cor=FALSE, Wald=TRUE)

Mult9 <- multinom(IncluML_disp ~ Sexo_Mujer + `Num_Total_disp` , data = Disponibles, trace= FALSE)
summary(Mult9, cor=FALSE, Wald=TRUE)

Mult10 <- multinom(IncluML_disp ~ Sexo_Mujer + `Edad_disp` , data = Disponibles, trace= FALSE)
summary(Mult10, cor=FALSE, Wald=TRUE)

Mult11 <- multinom(IncluML_disp ~ Sexo_Mujer + `exp_disp` , data = Disponibles, trace= FALSE)
summary(Mult11, cor=FALSE, Wald=TRUE)

Mult12 <- multinom(IncluML_disp ~ Sexo_Mujer + `exp2_disp` , data = Disponibles, trace= FALSE)
summary(Mult12, cor=FALSE, Wald=TRUE)

Mult1_1 <- update(Mult10, IncluML_disp ~ Sexo_Mujer , data=na.omit(Disponibles[ , all.vars(formula(Mult10))]))

anova(Mult1, Mult2)
anova(Mult1, Mult3)
anova(Mult1, Mult4)
anova(Mult1, Mult5)
anova(Mult1, Mult6) ##Mejor contraste- menor varianza##
anova(Mult1, Mult7)
anova(Mult1, Mult8)
anova(Mult1, Mult9)
anova(Mult1_1, Mult10)
anova(Mult1_1, Mult11)
anova(Mult1_1, Mult12)




##Comparacion 2 Ronda##

Mult61 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` , data = Disponibles, trace= FALSE)
summary(Mult61, cor=FALSE, Wald=TRUE)

Mult62 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc` , data = Disponibles, trace= FALSE)
summary(Mult62, cor=FALSE, Wald=TRUE)

Mult63 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Tamaño_1` +`Tamaño_2` , data = Disponibles, trace= FALSE)
summary(Mult63, cor=FALSE, Wald=TRUE)

Mult64 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `RM_1` +`RM_2` , data = Disponibles, trace= FALSE)
summary(Mult64, cor=FALSE, Wald=TRUE)

Mult65 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Tec_1` , data = Disponibles, trace= FALSE)
summary(Mult65, cor=FALSE, Wald=TRUE)


Mult66 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Num_Total_disp`, data = Disponibles, trace= FALSE)
summary(Mult66, cor=FALSE, Wald=TRUE)

Mult67 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + Edad_disp , data = Disponibles, trace= FALSE)
summary(Mult67, cor=FALSE, Wald=TRUE)


Mult68 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `exp_disp`, data = Disponibles, trace= FALSE)
summary(Mult68, cor=FALSE, Wald=TRUE)

Mult69 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult69, cor=FALSE, Wald=TRUE)

Mult6_1 <- update(Mult68, IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` , data=na.omit(Disponibles[ , all.vars(formula(Mult68))]))


anova(Mult6, Mult61) ##Modelo con menor varianza##
anova(Mult6, Mult62)
anova(Mult6, Mult63)
anova(Mult6, Mult64)
anova(Mult6, Mult65)
anova(Mult6, Mult66)
anova(Mult6_1, Mult67)
anova(Mult6_1, Mult68)
anova(Mult6_1, Mult69)

##Comparacion 3 Ronda##

Mult611 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc`  , data = Disponibles, trace= FALSE)
summary(Mult611, cor=FALSE, Wald=TRUE)

Mult612 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Tamaño_1` +`Tamaño_2`  , data = Disponibles, trace= FALSE)
summary(Mult612, cor=FALSE, Wald=TRUE)

Mult613 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `RM_1` +`RM_2`  , data = Disponibles, trace= FALSE)
summary(Mult613, cor=FALSE, Wald=TRUE)

Mult614 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Tec_1`  , data = Disponibles, trace= FALSE)
summary(Mult614, cor=FALSE, Wald=TRUE)

Mult615 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc`  , data = Disponibles, trace= FALSE)
summary(Mult615, cor=FALSE, Wald=TRUE)

Mult616 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Tamaño_1` +`Tamaño_2`  , data = Disponibles, trace= FALSE)
summary(Mult616, cor=FALSE, Wald=TRUE)

Mult617 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `RM_1` +`RM_2`  , data = Disponibles, trace= FALSE)
summary(Mult617, cor=FALSE, Wald=TRUE)

Mult618 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Tec_1` , data = Disponibles, trace= FALSE)
summary(Mult618, cor=FALSE, Wald=TRUE)

Mult61_1 <- update(Mult615, IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2`+ `Edad_disp` , data=na.omit(Disponibles[ , all.vars(formula(Mult615))]))

anova(Mult61, Mult611) ##Modelo con menor varianza##
anova(Mult61, Mult612)
anova(Mult61, Mult613)
anova(Mult61, Mult614)
anova(Mult61_1, Mult615)
anova(Mult61_1, Mult616)
anova(Mult61_1, Mult617)
anova(Mult61_1, Mult618)

##Comparacion 3 Ronda##

Mult6111 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc` + `exp_disp` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult6111, cor=FALSE, Wald=TRUE)

Mult6121 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Tamaño_1` +`Tamaño_2` + `exp_disp` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult6121, cor=FALSE, Wald=TRUE)

Mult6131 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `RM_1` +`RM_2`   + `exp_disp` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult6131, cor=FALSE, Wald=TRUE)

Mult6141 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_18-29` +`Edad_30-39`+ `Edad_40-49` + `Tec_1` + `exp_disp` + `exp2_disp` , data = Disponibles, trace= FALSE)
summary(Mult6141, cor=FALSE, Wald=TRUE)

Mult6151 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc`+ `exp_disp` + `exp2_disp`  , data = Disponibles, trace= FALSE)
summary(Mult6151, cor=FALSE, Wald=TRUE)

Mult6161 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Tamaño_1` +`Tamaño_2`+ `exp_disp` + `exp2_disp`  , data = Disponibles, trace= FALSE)
summary(Mult6161, cor=FALSE, Wald=TRUE)

Mult6171 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `RM_1` +`RM_2`  + `exp_disp` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult6171, cor=FALSE, Wald=TRUE)

Mult6181 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Tec_1` + `exp_disp` + `exp2_disp`, data = Disponibles, trace= FALSE)
summary(Mult6181, cor=FALSE, Wald=TRUE)

Mult611_1 <- update(Mult6151, IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2`+ `Edad_disp` , data=na.omit(Disponibles[ , all.vars(formula(Mult615))]))

anova(Mult61, Mult6111) ##Modelo con menor varianza##
anova(Mult61, Mult6121)
anova(Mult61, Mult6131)
anova(Mult61, Mult6141)
anova(Mult611_1, Mult6151)
anova(Mult611_1, Mult6161)
anova(Mult611_1, Mult6171)
anova(Mult611_1, Mult6181)

##Comparacion 4 Ronda##

Mult61511 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc`+ `exp_disp` + `exp2_disp` +`Num_Total_disp` , data = Disponibles, trace= FALSE)
summary(Mult61511, cor=FALSE, Wald=TRUE)

Mult61611 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `exp_disp` + `exp2_disp` +`Num_Total_disp` , data = Disponibles, trace= FALSE)
summary(Mult61611, cor=FALSE, Wald=TRUE)

Mult61711 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `RM_1` +`RM_2`  + `exp_disp` + `exp2_disp`+`Num_Total_disp`, data = Disponibles, trace= FALSE)
summary(Mult61711, cor=FALSE, Wald=TRUE)

Mult61811 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + `Edad_disp` + `Tec_1` + `exp_disp` + `exp2_disp`+`Num_Total_disp`, data = Disponibles, trace= FALSE)
summary(Mult61811, cor=FALSE, Wald=TRUE)

Mult611_1 <- update(Mult6151, IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2`+ `Edad_disp` , data=na.omit(Disponibles[ , all.vars(formula(Mult615))]))

anova(Mult61, Mult6111) ##Modelo con menor varianza##
anova(Mult61, Mult6121)
anova(Mult61, Mult6131)
anova(Mult61, Mult6141)
anova(Mult611_1, Mult61511)
anova(Mult611_1, Mult61611)
anova(Mult611_1, Mult61711)
anova(Mult611_1, Mult61811)

##Por tanto el modelo con mejor ajuste es el que incluye las variables edad y tamaño del hogar continuas##
####
Multfinal1 <- multinom(IncluML_disp ~ Sexo_Mujer + `IDE_1` +`IDE_2` + Edad_disp +  exp_disp+ exp2_disp+ `Escol_Secundaria` +`Escol_Prepa-Bachill`+ `Escol_Lic.Maest.Doc`+`Num_Total_disp`, data = Disponibles, trace= FALSE)
summary(Multfinal1, cor=FALSE, Wald=TRUE)
zMultfinal1 <- summary(Multfinal1)$coefficients/summary(Multfinal1)$standard.errors
pMultfinal1 <- (1-pnorm(abs(zMultfinal1),0,1))*2
coefMultfinal1 <- coef(Multfinal1)
exp(coefMultfinal1)
##ic <- confint(Multfinal1)##
##exp(ic)##
pMultfinal1





    ##GRAFICAS##
###################################################################################################################################################
##install.packages("Rmisc")##
##install.packages("lattice")##
##install.packages("plyr")##
##install.packages("descr")##
##install.packages("gtools")##
library(lattice)
library(plyr)
library(Rmisc) 

library(descr)
library(gtools)

library(tidyr)
library(dplyr)
freq(genero$P32, plot = FALSE)
library(data.table)

attach(Disponibles)
plot(IncluML_disp ~ Edad_disp)
plot(Sexo_disp ~ Edad_disp)



Table1 <- ftable(Disponibles[, c( "Group_edad_disp", "IncluML_disp")])

Table1  <- as.data.frame(Table1)
Edad <- transform(Table1,
                  Acum=cumsum(Freq),
                  Proporcion=round(prop.table(Freq),2),
                  Prop.Acum=round(cumsum(prop.table(Freq)),2))
library(ggplot2)
library(lubridate)
colors()
ggplot(Edad, aes(x=Group_edad_disp, y=Proporcion, color=IncluML_disp, group=IncluML_disp))+
  geom_point(size=2)+
  geom_line(size=2)+
  labs(title="Proporción de inclusión al ML por grupo de edad",
       subtitle = "¿Cuál es la corelación entre la edad y la probabilidad de regresar al ML? ",
       x="Grupos de edad",
       y="Proporción (%)",
       caption = "Fuente: elaboración de los autores con base en ECovid" ) +
  scale_color_manual(values=c("gray60", "gray30", "gray20"))+
  theme(legend.key = element_rect(fill = "white")) +
  theme_minimal()

ggplot(Edad, aes(x=Group_edad_disp, y=Proporcion, color=IncluML_disp, group=IncluML_disp))+
  labs(title="Proporción de inclusión al ML por grupo de edad",
       subtitle = "¿Cuál es la corelación entre la edad y la probabilidad de regresar al ML? ",
       x="Grupos de edad",
       y="Proporción (%)",
       caption = "Fuente: elaboración de los autores con base en ECovid" ) +
  scale_color_manual(values=c("gray60", "gray30", "gray20"))+
  geom_smooth(alpha = 0.2, size = 1.5, span = 4, se=FALSE)+
  theme(legend.key = element_rect(fill = "white")) +
  theme_minimal()


##Tablas##        

Disponibles <- Disponibles %>% 
  mutate(
    Regresa=case_when(
      IncluML_disp== "Regresa"~1,
      IncluML_disp!="Regresa"~0),
    No_Regresa=case_when(
      IncluML_disp=="No regresa"~1,
      IncluML_disp!="No regresa"~0),
    Disponible=case_when(
      IncluML_disp=="Disponible"~1,
      IncluML_disp!="Disponible"~0)
  )

table(Disponibles$Regresa)
table(Disponibles$IncluML_disp)
##Gra1##                     

Tab_1 <- Disponibles %>%
  group_by(Sexo_disp) %>%
  summarize(Mean_regresa=(mean(Regresa)*100),
            Mean_NOregresa=(mean(No_Regresa)*100),
            Mean_Dispobibles=(mean(Disponible)*100),
            Mean_regresara=(mean(IncluMLB_disp)*100)
  )

Tab_1<-   Tab_1 %>%  
  pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")


##Grap 1##
df1 <- summarySE(Disponibles, measurevar="Regresa", groupvars=c("Sexo_disp"), na.rm=T)   
df1 <- df1%>% mutate(IncluML=case_when(Sexo_disp=="Hombre"| Sexo_disp=="Mujer"~"1-Regresa"))
df1 <- df1 %>% rename(IncluMLB_disp=`Regresa`)
setnames(df1 , "Regresa", "IncluMLB_disp")


df2 <- summarySE(Disponibles, measurevar=c("No_Regresa"), groupvars=c("Sexo_disp"), na.rm=T)   
df2 <- df2%>% mutate(IncluML=case_when(Sexo_disp=="Hombre"| Sexo_disp=="Mujer"~"2-No_Regresa"))
setnames(df2 , "No_Regresa", "IncluMLB_disp")


df3 <- summarySE(Disponibles, measurevar=c("Disponible"), groupvars=c("Sexo_disp"), na.rm=T)   
df3 <- df3%>% mutate(IncluML=case_when(Sexo_disp=="Hombre"| Sexo_disp=="Mujer"~"3-Disponibles"))
setnames(df3 , "Disponible", "IncluMLB_disp")


Grap_1 = smartbind(df1, df2, df3)

Grap1 <-  ggplot(Grap_1, aes(x=IncluML , y=IncluMLB_disp, fill=Sexo_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
   geom_errorbar(aes(ymin=IncluMLB_disp-ci, ymax=IncluMLB_disp+ci), width=0.2,
                 position = position_dodge(.9)) +
      labs(title="Proporción de población disponible por sexo",
           subtitle = "Mexico, Abril-2020 ",
           x="Condición de los disponibles",
           y="Proporción(%)", 
           caption = "Fuente: elaboración propia con base en Ecovid")+
           theme_classic()

legend_title <- "Sexo" 
Grap1 + scale_fill_manual(legend_title,values=c("gray50", "gray25")) 
  

##Grap 2##
  
df4 <- summarySE(Disponibles, measurevar="Regresa", groupvars=c( "Escolaridad_disp"), na.rm=T)   
df4 <- df4%>% mutate(IncluML=case_when(Escolaridad_disp=="Primaria"| Escolaridad_disp!="Primaria"~"1-Regresa"))
setnames(df4 , "Regresa", "IncluMLB_disp")

df5 <- summarySE(Disponibles, measurevar=c("No_Regresa"), groupvars=c( "Escolaridad_disp"), na.rm=T)   
df5<- df5%>% mutate(IncluML=case_when(Escolaridad_disp=="Primaria"| Escolaridad_disp!="Primaria"~"2-No_Regresa"))
setnames(df5 , "No_Regresa", "IncluMLB_disp")

df6 <- summarySE(Disponibles, measurevar=c("Disponible"), groupvars=c("Escolaridad_disp"), na.rm=T)   
df6 <- df6%>% mutate(IncluML=case_when(Escolaridad_disp=="Primaria"| Escolaridad_disp!="Primaria"~"3-Disponibles"))
setnames(df6 , "Disponible", "IncluMLB_disp")

Grap_2 = smartbind(df4, df5, df6)


Grap2 <-   ggplot(Grap_2, aes(x=IncluML , y=IncluMLB_disp, fill=Escolaridad_disp)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=IncluMLB_disp-ci, ymax=IncluMLB_disp+ci), width=0.2,
                position = position_dodge(.9)) +
  labs(title="Proporción de población disponible por nivel de escolaridad",
       subtitle = "Mexico, Abril-2020 ",
       x="Condición de los disponibles",
       y="Proporción(%)", 
       caption = "Fuente: elaboración propia con base en Ecovid")+
  theme_classic()

legend_title <- "Escolaridad" 
Grap2 + scale_fill_manual(legend_title,values=c("gray50", "gray35", "gray20" , "gray10")) 



##Grap 3##
table(Disponibles$Num_Total1_disp)
df7 <- summarySE(Disponibles, measurevar="Regresa", groupvars=c( "Num_Total1_disp"), na.rm=T)   
df7 <- df7%>% mutate(IncluML=case_when(Num_Total1_disp==1| Num_Total1_disp!=1~"1-Regresa"))
setnames(df7 , "Regresa", "IncluMLB_disp")

df8 <- summarySE(Disponibles, measurevar=c("No_Regresa"), groupvars=c( "Num_Total1_disp"), na.rm=T)   
df8<- df8%>% mutate(IncluML=case_when(Num_Total1_disp==1| Num_Total1_disp!=1~"2-No_Regresa"))
setnames(df8 , "No_Regresa", "IncluMLB_disp")

df9 <- summarySE(Disponibles, measurevar=c("Disponible"), groupvars=c("Num_Total1_disp"), na.rm=T)   
df9 <- df9%>% mutate(IncluML=case_when(Num_Total1_disp==1| Num_Total1_disp!=1~"3-Disponibles"))
setnames(df9 , "Disponible", "IncluMLB_disp")


Grap_3 = smartbind(df7, df8, df9)


Grap3  <- ggplot(Grap_3, aes(x=Num_Total1_disp, y=IncluMLB_disp, color=IncluML, group=IncluML))+
  geom_point(size=2)+
  geom_line(size=2)+
  labs(title="Proporción de inclusión al ML por tamaño promedio de la vivienda",
       subtitle = "México, Abril 2020 ",
       x="Tamaño promedio",
       y="Proporción (%)",
       caption = "Fuente: elaboración de los autores con base en ECovid" ) +
  scale_color_manual(values=c("gray60", "gray30", "gray20"))+
  theme(legend.key = element_rect(fill = "white")) +
  theme_minimal()

legend_title <- "Categorias" 
Grap3 + scale_fill_manual(legend_title,values=c("gray60", "gray30", "gray20")) 


##Grap 4##

df10 <- summarySE(Disponibles, measurevar="Regresa", groupvars=c( "IDD1_disp"), na.rm=T)   
df10 <- df10%>% mutate(IncluML=case_when(IDD1_disp=="1"| IDD1_disp!="1"~"1-Regresa"))
setnames(df10 , "Regresa", "IncluMLB_disp")

df11 <- summarySE(Disponibles, measurevar=c("No_Regresa"), groupvars=c( "IDD1_disp"), na.rm=T)   
df11<- df11%>% mutate(IncluML=case_when(IDD1_disp=="1"| IDD1_disp!="1"~"2-No_Regresa"))
setnames(df11 , "No_Regresa", "IncluMLB_disp")

df12 <- summarySE(Disponibles, measurevar=c("Disponible"), groupvars=c("IDD1_disp"), na.rm=T)   
df12 <- df12%>% mutate(IncluML=case_when(IDD1_disp=="1"| IDD1_disp!="1"~"3-Disponibles"))
setnames(df12 , "Disponible", "IncluMLB_disp")

Grap_4 = smartbind(df10, df11, df12)
Grap_4 <- Grap_4%>% mutate( IDD1_disp=factor(IDD1_disp, levels =c("1","2","3","4") , labels = c("<0.25", "0.25-0.50", "0.50-0.75", "0.75-1.0")))

Grap4 <-     ggplot(Grap_4, aes(x=IncluML , y=IncluMLB_disp, fill=IDD1_disp)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=IncluMLB_disp-ci, ymax=IncluMLB_disp+ci), width=0.2,
                position = position_dodge(.9)) +
  labs(title="Proporción de población disponible por % de IDD",
       subtitle = "Mexico, Abril-2020 ",
       x="Condición de los disponibles",
       y="Proporción(%)", 
       caption = "Fuente: elaboración propia con base en Ecovid")+
       theme_classic()

legend_title <- "% Dependencia Demo" 
Grap4 + scale_fill_manual(legend_title,values=c("gray50", "gray35", "gray20" , "gray10")) 
######################################################################################################################























                     

Disponibles <- rename(Disponibles, Id_disp = Id, PER_disp = PER, Entidad_disp = ENT)


grapi2 <- ggplot(df3, aes(x=Sexo_disp, y=No_Regresa)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin=No_Regresa-ci, ymax=No_Regresa+ci), width=0.2,
                position = position_dodge(.9)) +
  labs(title="Proporción de población disponible por categorias",
       x="Condición de los disponibles",
       y="Proporción(%)")+
  scale_fill_manual(values=c("gray30", "gray5")) +
  theme_bw()
 
 
 
 
     
 grap2<-    ggplot(Tab_1, aes(x=Sexo_disp, y=Mean_regresara)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30", "gray5")) +
      theme_bw()
    
    ##Grap2##
    Tab_2 <- Disponibles %>%
      group_by(Group_edad_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_2<-   Tab_2 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    ggplot(Tab_2, aes(x=Condición, y=Proporción, fill=Group_edad_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30","gray20", "gray10", "gray5","gray1" )) +
      theme_bw()+
      coord_flip()
    
    ##Grap3##
    Tab_3 <- Disponibles %>%
      group_by(Escolaridad_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_3<-   Tab_3 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    ggplot(Tab_3, aes(x=Condición, y=Proporción, fill=Escolaridad_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30","gray20", "gray10", "gray5" )) +
      theme_bw()
      
    ##Grap4##
    Tab_4 <- Disponibles %>%
      group_by(Num_Total_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_4<-   Tab_4 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
 
    Tab_4<-  filter(Tab_4 , Num_Total_disp %in% c("1", "2","3", "4", "5", "6","7", "8", "9", "10", "11", "12"))  
    
    
    ggplot(Tab_4, aes(x=Num_Total_disp, y=Proporción, color=Condición))+
      geom_line(size=2)+
      geom_point(size=4)+
      labs(title="Proporción de población disponible por categorias por tamaño promedio del Hogar",
           x="Número promedio de personas por Hogar",
           y="Proporción (%)")+
      theme_minimal()+
      scale_linetype_manual(values=c("gray30", "gray10", "gray5" )) +
      theme(plot.title=element_text(hjust = 0.5, size = 10))
      
  
    ##Grap5##
    Disponibles <-  Disponibles %>% 
      mutate(
      IDD_disp=recode(IDD_disp, "Inf=0") )
    
    Tab_5 <- Disponibles %>%
      group_by(Escolaridad_disp) %>%
      summarize(Mean_IDD=(mean(IDD_disp))
       )
    
    Tab_5<-   Tab_5 %>%  
      M15_65_disp=recode(M15_65_disp, "1=1; NA=0")
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    
    ggplot(Tab_5, aes(x=IDD_disp, y=Proporción, color=Condición))+
      geom_line(size=2)+
      geom_point(size=4)+
      labs(title="Proporción de población disponible por categorias por tamaño promedio del Hogar",
           x="Número promedio de personas por Hogar",
           y="Proporción (%)")+
      theme_minimal()+
      scale_linetype_manual(values=c("gray30", "gray10", "gray5" )) +
      theme(plot.title=element_text(hjust = 0.5, size = 10))
    
    
    
 summary(Disponibles$Num_Total_disp)
 summary(Disponibles$IDD_disp)
 summary(Disponibles$RM_disp)
 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ggplot(Tab_3, aes(x=Condición, y=Proporción, fill=Escolaridad_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30","gray20", "gray10", "gray5" )) +
      theme_bw()  

      






