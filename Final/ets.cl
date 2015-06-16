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
(defglobal ?*question-11* = (create$ 1 2 3 4 5 6 7 8 9))

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
	(while (neq ?answer 9) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf)
  )
    (printout t "Insert " ?*question-11* ": ")
    (bind ?answer (read))
    (if (and (neq ?answer 9) (member ?answer ?*question-11*))
      then
        (assert (sintoma ?answer))
    )
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
    "What happends to you?"
    "I have an STD" ;; Module symtoms
    "I think I may have an STD" ;; Module no-symtoms
    "I would like to know more about STDs" ;; Module info
    "Bye"))
  (assert (response ?r))
  (watch facts)
)

(defmodule cambiar-a-inflamacion
  ?x <- (sintoma 1)
  =>
  (assert (modulo-inflamacion))
)

(defmodule cambiar-a-faringitis
  ?x <- (sintoma 2)
  =>
  (assert (modulo-faringitis))
)

(defmodule cambiar-a-ulcera
  ?x <- (sintoma 3)
  =>
  (assert (modulo-ulcera))
)

;; Asks for main sytoms of STDs
(defrule module11
  ?x <- (response 1)
=>
  (bind ?r (ask-question1
    "Selecciona algunos de los siguiente síntomas"
    "Dolor/escozor al orinar o al tener relaciones"
;    "Fluido amarillento"
;    "Dolor de testículos"
;   "Dolor ano-rectal"
;    "Sangrado"
;    "Tienes picor en tus partes"
    "Dolor de garganta"
    "Algún tipo de erupción o berruga"

    "Terminar"
  ))
)
;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO INFLAMACIONES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inflamaciones
?x<- (modulo-inflamaciones)
=>	
(bind ?r (ask-question1
    "Upps! Podrías presentar una infección! A continuación selecciona qué sintomas presentas:"
    "fluido amarillento y/o maloliente acompañado de dolor de testículos"
    "dolor anorectal y deseo de evacuar continuo"
    "sangrado rectal"
    "escozor y ardor tras en coito"
    "Terminar"
  ))
)
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
;?x <- (sintoma-inflamacion 2)	
?y <- (sintoma-inflamacion 3)
=>
(assert(infoproctitis))
)

;;;;;;;;;;;;;
;;BALANITIS;;
;;;;;;;;;;;;;

(defrule balanitis
;?x <- (sintoma-inflamacion 4)	
=>
(assert(infobalanitis))
)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO INFORMACION   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule informacion
?x<- (modulo-informacion)
=>	
(printout t "A continuación, te presentamos algunas descripciones de enfermades que, dado lo que nos has dicho" crlf
	    "podrías padecer. No olvides consultar con tu médico en caso de preocupación. ¡NUNCA TE AUTOMEDIQUES!" crlf
)
;;;;;;;;;;;;;
;;URETRITIS;;
;;;;;;;;;;;;;

(defrule INFORMAuretritis
?x<- (infouretritis)	
=>
(printout t "La Uretritis es una inflamación de la uretra, que puede ser causa O NO de una infección. " crlf
	    "Cuando no es causa de una infección, puede deberse a algún tipo de problema anatómico " crlf
            "como la estenosis o la fibrosis). Cuando sí lo es, puede deberse a la presencia de algún microorganismo." crlf
	    " Te tranquilizará saber que, en hombres sanos, se da en un (20%-30%) de los casos" crlf
)
)





;; Escribir en el menu varias posibles respuestas, en funcion de ellas, se ira preguntando sobre qué tipo de relación
;; ha tenido, y se irá saltando a los distintos modulos,  (Creo que un módulo por enfermedad estaria bien)


