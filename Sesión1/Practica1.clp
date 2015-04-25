(deffacts misHechos
	(hombre Pepe)
	(hombre Pablo)
	(hombre Fede)
	(hombre Alex)
	(hombre Guille)

	(mujer Mayte)
	(mujer Maria)
	(mujer Tere)
	(mujer Isa)
	(mujer Lidia)
	
	(casado Pepe Mayte)
	(casado Pablo "Maria Jose")
	(casado Fede Maria)

	(hijo-de Pepe Alex)
	(hijo-de Pepe Guille)
	(hijo-de Pablo Tere)
	(hijo-de Pablo Isa)
	(hijo-de Fede Mayte)
	(hijo-de Fede Lidia)
	(hijo-de Fede "Maria Jose")
)

; user input

(defrule pideNombre
	; sin antecedente para que se ejecute el primero
	=>
	(printout t "Escriba el nombre de un miembro de la familia: ")
	(assert (informacionSobre (read)))
)

(defrule padresDeInput
	(informacionSobre ?A)
	(padre-de ?A ?B)
	=>
	(printout t ?B " es padre de " ?A crlf)
)

(defrule hermanosDeInput
	(informacionSobre ?A)
	(hermanos ?B ?A)
	=>
	(assert (explica-hermano ?A ?B))
	(printout t ?B " es hermano de " ?A crlf)
)

(defrule explica-h
	(explica-hermano ?A ?B)
        (padre-de ?A ?C)
	(padre-de ?B ?C)
	=>
	(printout t ?A " es hermano de " ?B " porque ambos tienen como padre a " ?C crlf)
)

(defrule tiosDeInput
	(informacionSobre ?A)
	(tio-de ?A ?B)
	=>
	(printout t ?B " es tio de " ?A crlf)
)

(defrule primosDeInput
	(informacionSobre ?A)
	(primos ?A ?B)
	=>
	(printout t ?B " es primo de " ?A crlf)
)

(defrule hijosDeInput
	(informacionSobre ?A)
	(hijo-de ?A ?B)
	=>
	(printout t ?B " es hijo de " ?A crlf)
)

(defrule parejaDeInput
	(informacionSobre ?A)
	(casado ?A ?B)
	=>
	(printout t ?B " está casado con " ?A crlf)
)

(defrule abuelosDeInput
	(informacionSobre ?A)
	(abuelo-de ?A ?B)
	=>
	(printout t ?B " es abuelo de " ?A crlf)
)

(defrule suegrosDeInput
	(informacionSobre ?A)
	(suegro-de ?A ?B)
	=>
	(printout t ?B " es suego de " ?A crlf)
)

(defrule parejaDeInput
	(informacionSobre ?A)
	(casado ?A ?B)
	=>
	(printout t ?A " está casado con " ?B crlf)
)

(defrule nietosDeInput
	(informacionSobre ?A)
	(nieto-de ?A ?B)
	=>
	(printout t ?B " es nieto de " ?A crlf)
)

; ---------------------------------------------------------------------

(defrule deduceHermanos
	(hijo-de ?padre ?hijo)
	(hijo-de ?padre ?hijo2)
	(test (neq ?hijo ?hijo2)) ; Para que no sea yo mimo mi hermano
	=>
	(assert 
		(hermanos ?hijo ?hijo2)
		(padre-de ?hijo ?padre)
		(padre-de ?hijo2 ?padre)
	)
)

(defrule deduceHijos
	(hijo-de ?padre ?hijo)
	(casado ?padre ?madre)
	=>
	(assert (hijo-de ?madre ?hijo))
)

(defrule deduceSuegro
	(hijo-de ?A ?B)
	(casado ?B ?C)
	=>
	(assert (suegro-de ?C ?A))
)

(defrule deducePrimos
	(tio-de ?A ?B)
	(hijo-de ?B ?C)
	;(test (neq ?A ?C))
	=>
	(assert (primos ?A ?C))
)

(defrule deduceAbuelos
	(hijo-de ?abuelo ?padre)
	(hijo-de ?padre ?nieto)
	=>
	(assert 
		(abuelo-de ?nieto ?abuelo)
		(nieto-de ?abuelo ?nieto)
	)
)

(defrule deduceMatrimonios
	(casado ?padre ?madre)
	=>
	(assert (casado ?madre ?padre))
)

(defrule deduceTiosCasados
	(hermanos ?A ?B)
	(hijo-de ?A ?C)
	=>
	(assert (tio-de ?C ?B))
)

(defrule deduceTiosSolteros
	(casado ?A ?B)
	(tio-de ?C ?A)
	=>
	(assert (tio-de ?C ?B))
)
 
