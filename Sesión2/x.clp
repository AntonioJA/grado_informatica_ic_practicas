(deftemplate relacion
  (slot sintoma)
  (slot intensidad)
  (slot duracion)
)

(deftemplate hip
  (slot nombre)
)

;;; This function is used for every question made to the user.
;;; The question that is printed to the user is broken into three arguments (?qBEG ?qMID ?qEND) for flexibility, as we may need to include a printable in the middle.
;;; The argument $?allowed-values is a list that holds the allowed values that the program accepts.
;;; If the user enters a non-acceptable value, the program asks the question again until the answer is valid.

(deffunction ask-question (?qBEG ?qMID ?qEND $?allowed-values)

	(printout t ?qBEG crlf crlf)
  (printout t ?qMID crlf)
  (printout t "Introduce (" ?qEND"): ")
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer))
	)
	(while (not (member ?answer ?allowed-values)) do
    (printout t ?qBEG crlf crlf)
    (printout t ?qMID crlf)
    (printout t "Introduce (" ?qEND"): ")
  	(bind ?answer (read))
  	(if (lexemep ?answer)
  		then (bind ?answer (lowcase ?answer))
  	)
	)
?answer)

(defrule firstQuestion
  ?x <- (initial-fact)
	=>
  (retract ?x)
	(bind ?r (ask-question
    "¿Cuales son tus síntomas?"
    "Fiebre, Dolor de cabeza, tos, mal estar general"
    "fiebre, dolor, tos, mal, parar"
    fiebre dolor tos mal parar))
  (if (neq ?r parar)
		then (assert (initial-fact))
         (assert (relacion (sintoma ?r)))
  else
    (assert (duration))
    (retract ?x)
  )
  (facts)
)

(defrule duration
  ?d <- (duration)
  ?r <- (relacion (sintoma ?f))
  =>
  (retract ?r)
  (retract ?d)
  (printout t "Tienes " ?f ", ")
  (bind ?q (ask-question
    "¿Duración?"
    "dias, semanas, más"
    "dias, semanas, más"
    dias semanas mas))
	(assert (relacion (sintoma ?f) (duracion ?q)))
  (assert (intensity))
  (facts)
)

(defrule intensity
  ?i <- (intensity)
  ?r <- (relacion (sintoma ?f) (duracion ?d))
  =>
  (retract ?r)
  (retract ?i)
  (bind ?q (ask-question
    "Intensidad?"
    "normal, media, alta"
    "normal, media, alta"
    normal media alta))
	(assert (relacion (sintoma ?f) (duracion ?d) (intensidad ?q)))
  (assert (hipotesis))
  (facts)
)

;;
;; HIPOTESIS
;;

(defrule hip-meningitis
  (relacion (sintoma mal))
  =>
  (bind ?q (ask-question
    "¿Tienes rigidez en el cuello y molestias en la nuca?"
    "si/no"
    "si/no"
    si no))
  (if (eq ?q si)
    then
      (assert (relacion (sintoma rigidez)))
      (assert (hip (d meningitis) (p ?q)))
  )
  (facts)
)

(defrule hip-gripe
  (relacion (sintoma tos))
  (relacion (sintoma mal))
  (relacion (sintoma dolor))
  =>
  (assert (hip (d gripe) (p si)))
  (facts)
)

(defrule hip-denge
  (relacion (sintoma fiebre) (intensidad alta))
  =>
  (bind ?q (ask-question
    "¿Tienes ronchas?"
    "si/no"
    "si/no"
    si no))
  (assert (hip (d ronchas) (p ?q)))
  (facts)
)

(defrule hip-h1n1
  (hip (d gripe) (p si))
  (relacion (sintoma fiebre) (duracion mas))
  =>
  (bind ?q (ask-question
    "Epidemia de denge?"
    "si/no"
    "si/no"
    si no))
  (if (eq ?q si)
    then
      (assert (hip (d denge) (p si)))
      (assert (epidemia-denge))
    else
      (assert (hip (d h1n1) (p si)))
  )
  (facts)
)

;;
;; DIAGNÓSTICO
;;
(defrule diag-denge
  (hip (d ronchas) (p si))
  =>
  (printout t "Pruebas Denge" crlf)
)

(defrule diagnostico-meningitis
  (hip (d meningitis) (p si))
  (relacion (sintoma rigidez))
  (relacion (sintoma mal))
  =>
  (printout t "Pruebas meningitis" crlf)
)

(defrule diagnostico-h1n1
  (hip (d gripe) (p si))
  (not (epidemia-denge))
  (relacion (sintoma fiebre) (duracion mas))
  =>
  (printout t "Pruebas H1N1" crlf)
)
