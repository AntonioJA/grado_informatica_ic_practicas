;; Expert system to inform users about STDs

;; Templates
(deftemplate std "Template representing a STD"
    (slot std-name)
    (slot std-symtom)
    (slot std-symtom-where)
)

;; Facts
(deffacts Kwnoledge
  (std
    (std-name uretritis) (std-symtom inflamacion) )
  (std
    (std-name uretritis) (std-symtom coagulo) )
  (std
    (std-name uretritis) (std-symtom calculos) )
  (std
    (std-name uretritis) (std-symtom fluido) )
  (std
    (std-name uretritis) (std-symtom dolor-testiculos) )

  (std
    (std-name proctitis) (std-symtom inflamacion-recto) )
  (std
    (std-name proctitis) (std-symtom dolor-anorectal) )
  (std
    (std-name proctitis) (std-symtom sangrado) )
  (std
    (std-name proctitis) (std-symtom extrenimiento) )
  (std
    (std-name proctitis) (std-symtom pus) )

  (std
    (std-name balanitis) (std-symtom ardor) )
  (std
    (std-name balanitis) (std-symtom infeccion) )

  (std
    (std-name infec-faringeas) (std-symtom dolor-garganta) )
  (std
    (std-name infec-faringeas) (std-symtom inflamacion-ganglios-linf) )

  (std
    (std-name balanitis) (std-symtom candidas) )
  (std
    (std-name balanitis) (std-symtom prurito) )
  (std
    (std-name balanitis) (std-symtom ardor) )

  (std
    (std-name faringeas) (std-symtom dolor-garganta) )
  (std
    (std-name faringeas) (std-symtom inflamacion-ganglios-linf) )

  (std
    (std-name ulcera-genital) (std-symtom inflamacion-ganglios-ingles) )

   (std
    (std-name ulcera-genital) (std-symtom erupcion-pustula) )

  ;; Need more info.
  ;(std
  ;  (std-name verruga-genital) (std-symtom ) )

  (std
    (std-name ectoparasitosis) (std-symtom macula-roja) )
  (std
    (std-name ectoparasitosis) (std-symtom liendres-ladillas) )

  ;; Sífilis prematura
  (std
    (std-name sifilis) (std-symtom ulcera) )
  ;;Sífilis secundaria
  (std
    (std-name sifilis) (std-symtom roseola-rosa-palida-tronco) )
  (std
    (std-name sifilis) (std-symtom rojo-oscuro-platas) )
  (std
    (std-name sifilis) (std-symtom rojo-oscuro-zona-humeda) )
  (std
    (std-name sifilis) (std-symtom alopecia) )
)

;;; Global Variables
(defglobal ?*allowed-values* = (create$ 1 2 3))
(defglobal ?*question-11* = (create$ 1 2 3 4))
(defglobal ?*question-12* = (create$ 1 2 3 4 5))
(defglobal ?*yes-no* = (create$ 0 1))

;; Function used to ask questions to the user
;; @param ?qBEG: First part of the question
;; @param $?qMID: List of possible user answers
;; @return ?answer: Variable with the user input text
(deffunction ask-question (?qBEG $?qMID)
  (printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*allowed-values* ": ")
	(bind ?answer (read))

  (while (not (member ?answer ?*allowed-values*)) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf))
    (printout t "Insert " ?*allowed-values* ": ")
  	(bind ?answer (read))
	)
?answer)

;; Function used to ask questions to the user
;; @param ?qBEG: First part of the question
;; @param $?qMID: List of possible user answers
;; @return ?answer: Variable with the user input text
(deffunction ask-question1 (?qBEG $?qMID)
	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*question-11* ": ")
	(bind ?answer (read))
  (assert (sintoma ?answer))

  (while (neq ?answer 4) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf))
    (printout t "Insert " ?*question-11* ": ")
    (bind ?answer (read))
    (assert (sintoma ?answer))
	)
?answer)

(deffunction ask-question12 (?qBEG $?qMID)
	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*question-12* ": ")
	(bind ?answer (read))
  (assert (sintoma-inflamacion ?answer))

  (while (neq ?answer 5) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf))
    (printout t "Insert " ?*question-12* ": ")
    (bind ?answer (read))
    (assert (sintoma-inflamacion ?answer))
	)
?answer)

(deffunction ask-yesno-question (?qBEG $?qMID)
	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "(- ?field-index 1)  ") " ?field "." crlf))
  (printout t "Insert " ?*yes-no* ": ")
	(bind ?answer (read))

  (while (not (member ?answer ?*yes-no*)) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "(- ?field-index 1) ") " ?field "." crlf))
    (printout t "Insert " ?*yes-no* ": ")
    (bind ?answer (read))
	)
?answer)
;;;;;;;;;;;;;;;;;
;; Main Module ;;
;;;;;;;;;;;;;;;;;
;; The first thing we are going to do is ask the user who thinks he has.
(defrule firstQuestion
  ?x <- (initial-fact)
	=>
  (retract ?x)
	(bind ?r (ask-question
    "¿Qué te ocurre?"
    "Tengo una ETS" ;; Module symtoms
    "Creo que tengo una ETS" ;; Module no-symtoms
    "Me gustaría obtener información sobre las EFT" ;; Module info
    "Salir"))
  (assert (response ?r))
  (watch facts)
)

;; Asks for main sytoms of STDs
(defrule module11
  ?x <- (response 1)
=>
  (bind ?r (ask-question1
    "Selecciona algunos de los siguiente síntomas"
    "Dolor/escozor al orinar o al tener relaciones"
    "Dolor de garganta"
    "Algún tipo de erupción o berruga"
    "Terminar"
  ))
)

(defrule cambiar-a-inflamacion
  ?x <- (sintoma 1)
  =>
  (assert (modulo-inflamacion))
)

(defrule cambiar-a-faringitis
  ?x <- (sintoma 2)
  =>
  (assert (modulo-faringitis))
)

(defrule cambiar-a-ulcera
  ?x <- (sintoma 3)
  =>
  (assert (modulo-ulcera))
)

(defrule inflamaciones
  ?x<- (modulo-inflamacion)
=>
  (bind ?r (ask-question12
      "Parece que presentas una infección, si presentas alguno de éstos síntomas, selecciónalos."

      "Fluido amarillento y/o maloliente"
      "Dolor anorectal y deseo de evacuar continuo"
      "Sangrado rectal"
      "Escozor y ardor tras en coito"

      "Terminar"
    ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO FARINGITIS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule faringitis1
  (modulo-faringitis)
=>
  (bind ?r (ask-yesno-question
      "¿Has practicado últimamente sexo oral?"

      "No"
      "Sí"
    ))
    (assert (sintoma-faringitis-oral ?r))
)
(defrule faringitis11
  (modulo-faringitis)
  (sintoma-faringitis-oral 1)
=>
  (bind ?r (ask-yesno-question
      "¿Con más de una persona en los últimos meses?"

      "No"
      "Sí"
    ))
    (assert (sintoma-faringitis-mas-de-uno ?r))
)
(defrule faringitis13
  (modulo-faringitis)
  (sintoma-faringitis-oral 1)
  (sintoma-faringitis-mas-de-uno 0)
=>
  (bind ?r (ask-yesno-question
      "¿Ha dado tu pareja positivo en alguna prueba?"

      "No"
      "Sí"
    ))
    (assert (sintoma-faringitis-oral-pareja ?r))
)

(defrule exit-faringitis-normal
  ?ml <- (modulo-faringitis)
  ?x <- (sintoma-faringitis-oral 0)
=>
  (assert (faringitis-normal))
  (assert (modulo-informacion))
  (retract ?ml)
  (retract ?x)
)

(defrule exit-faringitis-mala
  ?ml <- (modulo-faringitis)
  ?x <- (sintoma-faringitis-oral 1)
  ?x1 <- (sintoma-faringitis-mas-de-uno ?y)
  ?x2 <- (sintoma-faringitis-oral-pareja 1)
=>
  (assert (faringitis-mala))
  (assert (modulo-informacion))
  (retract ?ml)
  (retract ?x)
  (retract ?x1)
  (retract ?x2)
)

(defrule exit-faringitis-normal-fallback
  ?ml <- (modulo-faringitis)
=>
  (assert (faringitis-normal))
  (assert (modulo-informacion))
  (retract ?ml)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO INFLAMACIONES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;;URETRITIS;;
;;;;;;;;;;;;;

(defrule uretritis
  ?x<- (sintoma-inflamacion 1)
=>
(assert(infouretritis))
)

;;;;;;;;;;;;;
;;PROCTITIS;;
;;;;;;;;;;;;;

(defrule proctitis
?x<- (sintoma-inflamacion 2)
=>
(assert(infoproctitis))
)

(defrule proctitis2
  ?x <- (sintoma-inflamacion 2)
  ?y <- (sintoma-inflamacion 3)
=>
  (assert(infoproctitis))
)

;;;;;;;;;;;;;
;;BALANITIS;;
;;;;;;;;;;;;;

(defrule balanitis
  ?x <- (sintoma-inflamacion 4)
=>
  (assert(infobalanitis))
)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO INFORMACION   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule informacion
  ?ml <- (modulo-informacion)
=>
  (printout t "A continuación, te presentamos algunas descripciones de enfermades que, dado lo que nos has dicho" crlf
	    "podrías padecer. No olvides consultar con tu médico en caso de preocupación. ¡NUNCA TE AUTOMEDIQUES!" crlf crlf)
  (retract ?ml)
)
;;;;;;;;;;;;;
;;URETRITIS;;
;;;;;;;;;;;;;

(defrule INFORMAuretritis
  ?x<- (infouretritis)
=>
  (printout t "La Uretritis es una inflamación de la uretra, que puede ser causa O NO de una infección. " crlf
	    "Cuando no es causa de una infección, puede deberse a algún tipo de problema anatómico " crlf
      "(como la estenosis o la fibrosis). Cuando sí lo es, puede deberse a la presencia de algún microorganismo." crlf
	    "En hombres sanos, se da en un (20%-30%) de los casos" crlf
	    "El periodo de incubación de la uretritis oscila entre 5 y 20 días, según el tipo de Uretritis. " crlf
	    "A pesar de que a veces puede presentar complicaciones, por lo general, no es una enfermedad grave." crlf
	    "Algunos síntomas observados en pacientes con Uretritis son: " crlf
	    "dolor al miccionar, fluido de aspecto claro y mucoso, con un color blanco o amarillo y olor fuerte y extraño," crlf
	    "dolor de testículos. Si presentas alguno d estos síntomas, consulta con tu médico para que te haga una prueba de uretritis" crlf crlf
  )
)

;;;;;;;;;;;;;
;;PROCTITIS;;
;;;;;;;;;;;;;

(defrule INFORMAproctitis
?x<- (infoproctitis)
=>
 (printout t "La Proctitis es una inflamación del recto, que puede venir O NO por tansmisión sexual. " crlf
	    "Las que no son transmitidas por vía sexual, son poco frecuentes." crlf
	  "Para diferenciarla entre si se debe a trasmisión sexual o no, hay que tener en cuenta las práticas sexuales del paciente. " crlf
	    "Algunos síntomas observados en pacientes con Proctitis son: " crlf
	    "dolor anorectal, secreción de moco o pus , incluso a veces pueden estar acompañados de sangrado rectal, " crlf
	    "deseo de evacuar constante y extreñimiento o diarrea." crlf
	    "La proctitis por transmisión sexual se da más en varones entre 15-30 años.En españa se diagnostican unos 2000 casos nuevos al año. "crlf
            "En países desarollados es más común en países de noreste de Europa y NorteAmérica" crlf crlf
  )
)

;;;;;;;;;;;;;
;;BALANITIS;;
;;;;;;;;;;;;;

(defrule INFORMAbalanitis
?x<- (infobalanitis)
=>
 (printout t "La balanitis es una infección por cándidas en el hombre. Es menos frecuente en los no circuncidados. " crlf
	    "No es grave, es muy frecuente y fácil de tratar.  " crlf
	    "Algunos síntomas observados en pacientes con Balanitis son: " crlf
	    "picor en la zona y sensación de ardor después del coito. " crlf crlf
  )
)

;;;;;;;;;;;;;;
;;FARINGITIS;;
;;;;;;;;;;;;;;

(defrule INFORMAfaringitisMala
  (faringitis-mala)
=>
 (printout t "La faringitis de origen infeccioso transmitida sexualmente es el resultado de las relaciones orogenitales." crlf
		"La infección se transmite más fácilmente tras una felación que después de un cunnilingus. Se trasmite tanto de la" crlf
 		"Boca a los genitales como de los genitales a la boca. " crlf
		"Algunos síntomas observados en pacientes con este tipo de Faringitis son: " crlf
		"Dolor de garganta producido al tragar fluidos, inflamación de los ganglios. " crlf
		"Sin embargo, es frecuentemente Asíntomática." crlf
		"Normalmente este tipo de faringitis se diagnostica como una faringitis normal (o faringitis de repetición)" crlf
		"Inicialmente, y si no se evoluciona bien al tratamiento habitual que se suele seguir, se trata de " crlf
		"Diagnosticar este tipo de faringitis, basándose en las prácticas sexuales del paciente. " crlf crlf)
)

(defrule INFORMAfaringitis
(faringitis-normal)
=>
 (printout t "La faringitis es la inflamación de la mucosa que reviste la faringe." crlf
	     "Generalmente le acompañan síntomas como deglución difícil, amígdalas inflamadas y fiebre más o menos elevada." crlf
 	     "Las posibles causas de la faringitis son las infecciones víricas, infecciones bacterianas " crlf
	     " o reacciones alérgicas. " crlf
	     " No suele ser grave. El tratamiento que se suele poner " crlf
	     " consiste en administración de líquidos y reposo, analgésicos y antinflamatorios (en general paracetamol,ibuprofeno..)" crlf
	     " o antisépticos chupados." crlf crlf
))




;; Escribir en el menu varias posibles respuestas, en funcion de ellas, se ira preguntando sobre qué tipo de relación
;; ha tenido, y se irá saltando a los distintos modulos,  (Creo que un módulo por enfermedad estaria bien)
