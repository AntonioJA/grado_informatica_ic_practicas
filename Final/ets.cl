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
(defglobal ?*question-12* = (create$ 1 2 3 4 5))

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

(deffunction ask-question12 (?qBEG $?qMID)

	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*question-12* ": ")
	(bind ?answer (read))
	(while (neq ?answer 5) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf)
  )
    (printout t "Insert " ?*question-12* ": ")
    (bind ?answer (read))
    (if (and (neq ?answer 9) (member ?answer ?*question-12*))
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
;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODULO INFLAMACIONES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule inflamaciones

  ?x<- (modulo-inflamacion)
=>
  (bind ?r (ask-question12
      "fluido amarillento y/o maloliente"
      "dolor anorectal y deseo de evacuar continuo"
      "sangrado rectal"
      "escozor y ardor tras en coito"
      "Terminar"
    ))
)

(defrule uretritis
  ?x<- (sintoma-inflamacion 1)
)

;; Escribir en el menu varias posibles respuestas, en funcion de ellas, se ira preguntando sobre qué tipo de relación
;; ha tenido, y se irá saltando a los distintos modulos,  (Creo que un módulo por enfermedad estaria bien)

;;;;;;;;;;;;;;;;
;; Tip Module ;;
;;;;;;;;;;;;;;;;
