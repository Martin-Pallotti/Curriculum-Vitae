clear all
set more off

cd "C:\Users\marto\OneDrive\Documentos\Maestría\2024\Primer cuatrimestre\Instrumentos Computacionales\Parcial\Stata"

use ".\data\Ejercicio 3\pnad14_padres.dta"

*Genero la variable categórica de educación de los padres

* creo una nueva variable de nivel educativo
gen nivel_ed_padres = .
replace nivel_ed_padres = 1 if nivel_parents == 0                /* Nunca asistio                             */
replace nivel_ed_padres = 2 if nivel_parents == 1                /* Primaria incompleta                       */
replace nivel_ed_padres = 3 if nivel_parents == 2 | nivel_parents== 3    /* Primaria completa o secundaria incompleta */
replace nivel_ed_padres = 4 if nivel_parents == 4 | nivel_parents== 5     /* Secundaria completa o superior incompleta */
replace nivel_ed_padres = 5 if nivel_parents == 6                /* Superior completa                         */

*Saco las variables de salario que sean igual a missing
keep if wage!=.

*Genero el logaritmo del salario
gen lwage = log(wage)

*Corro la regresión en base a lo pedido en el enunciado
reg lwage i.hombre i.urbano aedu i.djubila i.reg_uf i.nivel_ed_padres [w=pondera], robust

*grafico	 
margins i.nivel_ed_padres
marginsplot, lev(95) xlabel(1 "sin educ" 2 "PRII" 3 "PRIC/SECI" 4 "SECC/SUPI" 5 "SUPC", ang(v) labsize(small)) xtitle("Nivel educativo de los padres") ///
ytitle("Efecto en (log.) salarios") scheme(s1mono) /// 
title("Salarios por nivel educativo de los padres") note("Intervalos de confianza de 90%" "Notas: PRII (Primaria Incompleta), PRIC (Primaria Completa) " ///
"SECI (Secundaria Incompleta), SECC (Secundaria Completa)" "SUPI (Superior Incompleta), SUPC (Superior Completa)")
graph export ".\resultados\Ejercicio 3\sal_niv_ed_padres.png" , replace



*inciso c

*Utilizando la variable continua de año de educación de los padres, filtro si es mujer
reg lwage educ_parents i.urbano aedu i.djubila i.reg_uf [w=pondera] if hombre==0, robust
eststo b_mujeres /*guardo el coeficiente*/

*Utilizando la variable continua de año de educación de los padres, filtro si es hombre
reg lwage educ_parents i.urbano aedu i.djubila i.reg_uf [w=pondera] if hombre==1, robust
eststo b_hombres /*guardo el coeficiente*/

*Utilizando la variable continua de año de educación de los padres, filtro si zona rural
reg lwage educ_parents i.urbano aedu i.djubila i.reg_uf [w=pondera] if urbano==0, robust
eststo b_rural /*guardo el coeficiente*/

*Utilizando la variable continua de año de educación de los padres, filtro si es zona urbana
reg lwage educ_parents i.urbano aedu i.djubila i.reg_uf [w=pondera] if urbano==1, robust
eststo b_urbano /*guardo el coeficiente*/

*Grafico en base a los coeficientes guardados
coefplot (b_mujeres , keep(educ_parents) mcolor(red) ciopts(color(red)) msymbol(O) label("Mujeres")) ///
(b_hombres , keep(educ_parents) mcolor(midblue) ciopts(color(midblue)) msymbol(D) label("Hombres")) ///
(b_rural , keep(educ_parents) mcolor(green) ciopts(color(green)) msymbol(S) label("Rural")) ///
(b_urbano , keep(educ_parents) mcolor(orange) ciopts(color(orange)) msymbol(T) label("Urbano")), ///
scheme(s1mono) graphregion(color(white)) vert ytitle("Efecto de año adicional de" /// 
"educ.padres en (log.) salarios") xlabel(,nolabel) legend(col(4)) note("Nota: Intervalos de confianza de 90%")

*Guardo el gráfico
graph export ".\resultados\Ejercicio 3\sal_años_ed_padres.png", replace
