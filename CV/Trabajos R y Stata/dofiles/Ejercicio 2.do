****
**** Prepara la base de INDEC en formato STATA
****
clear all
set more off
*Establezco el directorio
cd "C:\Users\marto\OneDrive\Documentos\Maestría\2024\Primer cuatrimestre\Instrumentos Computacionales\Parcial\Stata"

*Armo la base de eph pero para 2019
local base_in_2   = ".\data\Ejercicio 2\Usu_hogar_T319.txt"
local base_in_1   = ".\data\Ejercicio 2\Usu_individual_T319.txt"
local base_out	  =	".\data\Ejercicio 2\eph_q32019.dta"


* Ordeno bases de hogar
tempfile t1
drop _all
insheet using "`base_in_2'", delimiter(";")
destring, replace 
duplicates report aglomerado codusu nro_hogar
sort aglomerado codusu nro_hogar
save `t1', replace

* Ordeno base de personas
drop _all
insheet using "`base_in_1'", delimiter(";")
destring, replace 
duplicates report aglomerado codusu nro_hogar componente
sort aglomerado codusu nro_hogar componente

* Junto ambas bases que gracias al ordenamiento se pueden fusionar
merge aglomerado codusu nro_hogar using `t1'
tab _merge
more

* Elimino las observaciones inconsistentes
drop if _merge==1
more
drop _merge

* Elimino observaciones duplicadas
duplicates report
more
duplicates drop
more

destring decindr, replace
destring decifr, replace
destring deccfr, replace


* Comprimo y guardo la base
compress
sort codusu nro_hogar trimestre /*id*/  componente /*com*/ , stable



*****
*****
*****

***Preparacion:
* 1. Genero identificador del hogar (id)
sort codusu nro_hogar trimestre 
egen id = group(codusu nro_hogar trimestre)  

* 2. Variable de genero
gen     hombre = 0		if  ch04==2
replace hombre = 1		if  ch04==1	

* 3. Variable de edad
gen 	edad = ch06 
replace edad = 0		if  edad==-1 
replace edad = .		if  edad==99 

* 4. Regiones
gen	region_est = "Gran Buenos Aires"			if  region==1			
replace	region_est = "Pampeana"			if  region==43 			
replace	region_est = "Cuyo"			if  region==42			
replace	region_est = "Noroeste Argentino"			if  region==40			 
replace	region_est = "Patagonia"			if  region==44				
replace	region_est = "Noreste Argentino"			if  region==41		

save "`base_out'", replace


*Ejercicio

*Importo la base de la eph 2019 previamente creada
use ".\data\Ejercicio 2\eph_q32019.dta", clear

*Genero la variable de muestra
gen muestra=.

*Genero las 12 muestras
replace muestra=1 if region==1 & hombre==0
replace muestra=2 if region==1 & hombre==1
replace muestra=3 if region==43 & hombre==0
replace muestra=4 if region==43 & hombre==1
replace muestra=5 if region==42 & hombre==0
replace muestra=6 if region==42 & hombre==1
replace muestra=7 if region==40 & hombre==0
replace muestra=8 if region==40 & hombre==1
replace muestra=9 if region==44 & hombre==0
replace muestra=10 if region==44 & hombre==1
replace muestra=11 if region==41 & hombre==0
replace muestra=12 if region==41 & hombre==1

*Identifico a las personas activas
gen activas=.
replace activas=1 if (estado==1 | estado==2 )
replace activas=0 if estado==3

*Identifico a las personas ocupadas
gen ocupados=.
replace ocupados=1 if estado==1 
replace ocupados=0 if (estado==2 | estado==3)

*Identifico a las personas desempleadas
gen desempleadas=.
replace desempleadas= 1 if estado==2
replace desempleadas= 0 if (estado==1 | estado==3)
*Calculo el ingreso promedio de la ocupación principal 
preserve

collapse p21 [w=pondiio], by(muestra region_est hombre)
	
rename p21 media_ing_op
tempfile p21
save `p21', replace

restore

*Calculo la tasa de activos, ocupados y desempleados	
preserve
	
collapse activas ocupados desempleadas [w=pondera], by(muestra)
	
rename activas tasa_activas
rename ocupados tasa_ocupados
rename desempleadas tasa_desempleadas

replace tasa_activas = tasa_activas*100
replace tasa_ocupados = tasa_ocupados*100
replace tasa_desempleadas = tasa_desempleadas*100

*La fusiono con la del ingreso para obtener una única tabla	
merge 1:1 muestra using `p21'
	
drop _merge
	
* Si queremos guardar en formato excel:
export excel ".\Resultados\inciso_c.xls", firstrow(variables) sheet("2019",replace)

restore


*Realizo el mismo procedimiento pero para el 2023
use ".\data\Ejercicio 2\eph_q32023.dta", clear

*Genero la variable de muestra
gen muestra=.

*Genero las 12 muestras
replace muestra=1 if region==1 & hombre==0
replace muestra=2 if region==1 & hombre==1
replace muestra=3 if region==43 & hombre==0
replace muestra=4 if region==43 & hombre==1
replace muestra=5 if region==42 & hombre==0
replace muestra=6 if region==42 & hombre==1
replace muestra=7 if region==40 & hombre==0
replace muestra=8 if region==40 & hombre==1
replace muestra=9 if region==44 & hombre==0
replace muestra=10 if region==44 & hombre==1
replace muestra=11 if region==41 & hombre==0
replace muestra=12 if region==41 & hombre==1

*Identifico a las personas activas
gen activas=.
replace activas=1 if estado>=0 & estado<=2 & (edad>=15 & edad<=65)
replace activas=0 if estado==4 | estado==3 & (edad>=15 & edad<=65)

*Identifico a las personas ocupadas
gen ocupados=.
replace ocupados=1 if estado==1 & (edad>=15 & edad<=65)
replace ocupados=0 if estado!=1 & (edad>=15 & edad<=65)

gen desempleadas=.
replace desempleadas= 1 if estado==2 & (edad>=15 & edad<=65)
replace desempleadas= 0 if estado!=2 & (edad>=15 & edad<=65)

*Calculo el ingreso promedio de la ocupación principal 
preserve

collapse p21 [w=pondiio], by(muestra region_est hombre)
	
rename p21 media_ing_op
tempfile p21
save `p21', replace

restore
	
*Calculo la tasa de activos, ocupados y desempleados	
preserve
	
collapse activas ocupados desempleadas [w=pondera], by(muestra region_est hombre)
	
rename activas tasa_activas
rename ocupados tasa_ocupados
rename desempleadas tasa_desempleadas

replace tasa_activas = tasa_activas*100
replace tasa_ocupados = tasa_ocupados*100
replace tasa_desempleadas = tasa_desempleadas*100
	
merge 1:1 muestra using `p21'
	
drop _merge
	
* En este caso, exporto en otra hoja en el mismo excel la información del 2023
export excel ".\Resultados\inciso_c.xls", firstrow(variables) sheet("2023",replace)

restore
