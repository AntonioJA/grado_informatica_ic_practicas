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

(deffunction ask-question31 (?qBEG $?qMID)
	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*question-11* ": ")
	(bind ?answer (read))
  (if (member ?answer ?*question-11*)
    then
      (assert (sintoma-ulcera ?answer))
  )
  (while (neq ?answer 4) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf))
    (printout t "Insert " ?*question-11* ": ")
    (bind ?answer (read))
    (if (member ?answer ?*question-11*)
      then
        (assert (sintoma-ulcera ?answer))
    )
	)
?answer)

(deffunction ask-averigua-sifilis (?qBEG $?qMID)
	(printout t crlf ?qBEG crlf crlf)
  (progn$ (?field $?qMID)
    (printout t "    "?field-index ") " ?field "." crlf))
  (printout t "Insert " ?*question-12* ": ")
	(bind ?answer (read))
  (if (member ?answer ?*question-12*)
    then
      (assert (sintoma-sifilis ?answer))
  )
  (while (neq ?answer 5) do
    (printout t crlf ?qBEG crlf crlf)
    (progn$ (?field $?qMID)
      (printout t "    "?field-index ") " ?field "." crlf))
    (printout t "Insert " ?*question-12* ": ")
    (bind ?answer (read))
    (if (member ?answer ?*question-12*)
      then
        (assert (sintoma-sifilis ?answer))
    )
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
    "Creo que tengo una ETS, porque tengo síntomas"
    "Creo que podría tener una ETS, pero no presento síntomas"
    "Me gustaría obtener información sobre las ETS"
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
    "Algún tipo de erupción/berruga/úlcera"

    "Terminar"
  ))
  (retract ?x)
)

;;(defrule module12
;;  ?x <- (response 2)
;;=>
;;
;;  (retract ?x)
;;)
;;
;;(defrule module13
;;  ?x <- (response 3)
;;=>
;;  (retract ?x)
;;)

(defrule cambiar-a-inflamacion
  ?x <- (sintoma 1)
  =>
  (assert (modulo-inflamacion))
  (retract ?x)
)

(defrule cambiar-a-faringitis
  ?x <- (sintoma 2)
  =>
  (assert (modulo-faringitis))
  (retract ?x)
)

(defrule cambiar-a-ulcera
  ?x <- (sintoma 3)
  =>
  (assert (modulo-ulcera))
  (retract ?x)
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
  (assert (modulo-informacion))
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   MODULO ÚLCERA     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule ask-ulcera
  (modulo-ulcera)
=>
  (bind ?r (ask-question31
      "¿Presentas alguno de éstos síntomas?"

      "Verruga en los genitales"
      "Liendres o ladillas"
      "Úlcera o erupción cutánea"

      "Terminar"
    ))
    (assert (modulo-informacion))
)

(defrule ulcera1
  ?ml <- (modulo-ulcera)
  (sintoma-ulcera 1)
=>
  (bind ?r (ask-yesno-question
    "¿Has tenido alguna relación de riesgo?"

    "No"
    "Sí"
  ))
  (assert (sintoma-ulcera-riesgo ?r))
  (retract ?ml)
)

(defrule ulcera2
  ?ml <- (modulo-ulcera)
  (sintoma-ulcera 2)
=>
  (bind ?r (ask-yesno-question
    "¿Te pica?"

    "No"
    "Sí"
  ))
  (assert (sintoma-ulcera-liendre ?r))
  (retract ?ml)
)

(defrule ulcera3
  ?ml <- (modulo-ulcera)
  (sintoma-ulcera 3)
=>
  (bind ?r (ask-yesno-question
    "¿Has estado, os eres de un pais Tropical?"

    "No"
    "Sí"
  ))
  (assert (sintoma-ulcera-tropical ?r))
  (retract ?ml)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; ULCERA GENITAL ;;
;;;;;;;;;;;;;;;;;;;;

(defrule ulcera
  (sintoma-ulcera-tropical 0 )
  =>
  (assert(infoUlcera))
)

(defrule ulceraMala
  (sintoma-ulcera-tropical 1 )
=>
  (assert(infoUlceraMala))
)

(defrule ulceraMalaTipo
  (sintoma-ulcera-tropical 1)
  (sintoma-ulcera 3)
=>
  (bind ?r (ask-averigua-sifilis
    "¿De qué tipo es tu úlcera?"

    "Color rosa-pálido"
    "Rojo-oscuro en tronco, extremidades, planta o palmas"
    "Rojo-oscuro, area genital, perineo, ingles, axilas, zonas húmedas o de pliegues"
    "Caido del pelo por zonas"

    "Terminar"
  ))
  (assert (averigua-sifilis))
)

(defrule sifilis1
    (sintoma-sifilis 1)
  =>
    (assert (sifilis-roseola))
)
(defrule sifilis2
    (sintoma-sifilis 2)
  =>
    (assert (sifilis-papulosa))
)
(defrule sifilis3
    (sintoma-sifilis 3)
  =>
    (assert (sifilis-condimlomas))
)
(defrule sifilis4
    (sintoma-sifilis 4)
  =>
    (assert (sifilis-alopecia))
)

;;;;;;;;;;;;;;;;;;;;;;;;
;; VERRUGAS GENITALES ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defrule verrugas
(sintoma-ulcera-riesgo 1 )
=>
(assert(infoVerrugaMala))
)

(defrule verrugas
(sintoma-ulcera-riesgo 0 )
=>
(assert(infoVerruga))
)

;;;;;;;;;;;;;;;;;;;;;
;; ECTOPARASITOSIS ;;
;;;;;;;;;;;;;;;;;;;;;

(defrule esteroparasitosis
(sintoma-ulcera-liendre 1 )
=>
(assert(infoesteroparasitosis))
)

(defrule esteroparasitosis
(sintoma-ulcera-liendre 1 )
=>
(assert(infoCostrasPrurito))
)

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
(defrule proctitis2
  ?x <- (sintoma-inflamacion 2)
  ?y <- (sintoma-inflamacion 3)
=>
  (assert(infoproctitis))
)

(defrule proctitis
?x<- (sintoma-inflamacion 2)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ULCERA NORMAL (HERPES)      ;;
;;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;
(defrule INFORMAherpes
  (infoUlcera)
=>
  ( printout t
  "El herpes es una infección causada por un virus herpes simple (VHS).
   El herpes bucal provoca llagas alrededor de la boca o en el rostro.
    El herpes genital es una enfermedad de transmisión sexual (ETS).
     Puede afectar los genitales, las nalgas o el área del ano.
     Otras infecciones por herpes pueden afectar los ojos, la piel u otras partes del cuerpo. El virus puede ser peligroso en recién nacidos o en personas con sistemas inmunes debilitados." crlf crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ULCERA FEA                  ;;
;;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;
(defrule INFORMAulcera
  (infoUlceraMala)
=>
  (printout t
    "Una causa común de las úlceras en los genitales masculinos son las infecciones de transmisión a través del contacto sexual, como por ejemplo:" crlf crlf

    "   *) Herpes genital (ampollas pequeñas y dolorosas llenas de líquido claro o color paja)." crlf
    "   *) Verrugas genitales (manchas de color carne que son elevadas o planas y pueden lucir como la parte superior de una coliflor)." crlf
    "   *) Chancroide (una pequeña protuberancia en los genitales, que se convierte en una úlcera al cabo de un día de su aparición)." crlf
    "   *) Sífilis (llaga o úlcera abierta pequeña e indolora, llamada chancro, en los genitales)." crlf
    "   *) Granuloma inguinal (protuberancias pequeñas de color carne que aparecen en los genitales o alrededor del ano)." crlf
    "   *) Linfogranuloma venéreo (pequeña llaga indolora en los genitales masculinos)." crlf crlf)
)
;;;;;;;;;;;;;;;;;;;;;
;;VERRUGAS NORMALES;;
;;;;;;;;;;;;;;;;;;;;;

(defrule INFORMAVerrugaNormal
  (infoVerruga)
=>
  (printout t
      "La verruga es una lesión cutánea causada por el virus del papiloma humano." crlf
      "presentan una forma variable, llamativa y por lo general, de forma globular," crlf
      "y pueden afectar a distintas zonas de la piel." crlf crlf
      "Su extirpación no es fácil ya que las verrugas tienen su propio sistema de irrigación sanguínea que causan sangramientos abundantes cuando su extracción es por medios no clínicos; además pueden regenerarse con mayor virulencia. Adicionalmente compromete varios terminales nerviosos por lo que su extracción o manipulación causa gran dolor" crlf
      "Pueden contraerse por contacto íntimo con personas afectadas por el virus del papiloma humano radicado en la zona genital y por transmisión consanguínea de portadores asintomáticos. El desarrollo de verrugas se favorece cuando hay fallos en el sistema inmunitario." crlf crlf
      )
)

;;
;;Dependiendo del serotipo del virus, la zona afectada es distinta:[cita requerida]
;;
;;    las manos,
;;    la cara,
;;    la nuca,
;;    los pies,
;;    la zona ano-genital,
;;    las axilas, o cualquier otra parte del cuerpo.

;;;;;;;;;;;;;;;;;;;;
;; VERRUGAS FEAS  ;;
;;;;;;;;;;;;;;;;;;;;

(defrule INFORMAVerrugaFea
  (infoVerrugaMala)
=>
  (printout t
    "La verruga es una lesión cutánea causada por el virus del papiloma humano." crlf
    "presentan una forma variable, llamativa y por lo general, de forma globular," crlf
    "y pueden afectar a distintas zonas de la piel" crlf)
)

;;;;;;;;;;;;;;;;;;;;;;
;; ECTEROPARASITOSIS;;
;;;;;;;;;;;;;;;;;;;;;;


(defrule INFORMAEctoparasitosis
  (infoesteroparasitosis)
=>
  (printout t
    "La ectoparasitosis es una dermatosis parasitaria (Los parasitos se deslizan por encima o debajo de la piel.)" crlf
    "Se hallan muy extendidas por el mundo." crlf
    "Son comunes en presonas que se encuentran en situaciones precarias" crlf crfl)
)
