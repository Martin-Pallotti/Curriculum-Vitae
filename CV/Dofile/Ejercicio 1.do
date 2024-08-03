/*  ----------    Ejercicio 1    --------------           */

clear all

set more off

*Establezco mi directorio
cd "C:\Users\marto\OneDrive\Documentos\Maestría\2024\Primer cuatrimestre\Instrumentos Computacionales\Parcial\Stata"

*--------------- Inciso a ----------

*Importo la base
import excel using ".\Entrega\Data\EDU-2019-4229-EN-T008.xlsx", sheet("Table II.B1.3.1") cellrange(A14:S96) firstrow clear

*Hago las modificaciones necesarias para que me quede una base de fácil ejecución
rename A pais

drop in 1

drop in 42
drop in 46

drop s G J M P S

*Mediante los siguientes códigos me creo variables numéricas
encode Meanscore, gen (mean_score_bq) 
encode SE, gen(se_bq)

encode E ,gen(mean_score_thq)
encode F , gen(se_thq)

encode H , gen(mean_score_sq)
encode I, gen(se_sq)

encode K, gen(mean_score_tq)
encode L, gen(se_tq)

encode Scoredif, gen(scoredif_t_b)
encode O ,gen(se_t_b)

encode Q, gen(percentage)
encode R, gen(se_percentage)

*Elimino las variables de tipo string
drop Meanscore-R 

*Los países elegidos son: Dinamarca, Finlandia, Colombia, Israel, Argentina, Brazil, Peru, Uruguay, Canada, Noruega, Costa Rica, República Dominicana, Francia ///
*Estados Unidos, Reino Unido, Suecia, Portugal, Italia, Panama y Alemania.

*Adapto los nombres para que sean los mismo en ambas bases
replace pais= "Portugal" if pais=="Portugal*"
replace pais= "United States" if pais=="United States*"

*Me quedo con 20 países
keep if pais=="Denmark" | pais== "Finland" | pais== "Colombia" | pais== "Israel" | pais== "Argentina" | pais== "Brazil" | pais== "Peru" | pais== "Uruguay" ///
 | pais== "Canada" | pais== "Norway" | pais== "Costa Rica" | pais== "Dominican Republic" | pais== "France" | pais== "United States" | pais== "United Kingdom" | ///
 pais=="Sweden" | pais== "Portugal" | pais== "Italy" | pais=="Panama" | pais == "Germany"

*Guardo en una base temporal
tempfile educ_20
save `educ_20', replace

* ------------- Inciso b ------------------

*Importo la base del WB
import excel using ".\Entrega\Data\Data_WB.xlsx", sheet("Data") cellrange(A1:E21) firstrow clear

*Elimino las variables que no son necesarias
drop CountryCode-SeriesCode

*Renombro las variables
rename CountryName pais

*Guardo en una base temporal
tempfile paises_gini
save `paises_gini', replace

*Fusiono las bases
merge 1:m pais using `educ_20'

*Elimino la variable que no necesito
drop _merge 

*Genero el gráfico
twoway (scatter  scoredif_t_b YR2019     ,title("Diferencia Top-Bottom - Indice de Gini") scheme(s1color) ///
ytitle("Diferencia Top-Bottom") xtitle("Indice de Gini") ///
msymbol(circle) mcolor(blue) msize(medium)) ///
(function y=x, range(0 60) legend(off))

graph export ".\Entrega\Resultados\ej1.png",replace







	

	
