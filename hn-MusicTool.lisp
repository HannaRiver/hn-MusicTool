;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Music Tools;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Global Variables;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defConstant Cm-score-midi-maps '(59 60 62 64 65 67 69))
;(setq bpm 120)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Key C Dictionary;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf C-pitch-to-name (make-hash-table))
(setf (gethash '60 C-pitch-to-name) 'C)
(setf (gethash '61 C-pitch-to-name) 'uC) 
(setf (gethash '62 C-pitch-to-name) 'D)
(setf (gethash '63 C-pitch-to-name) 'uD)
(setf (gethash '64 C-pitch-to-name) 'E)
(setf (gethash '65 C-pitch-to-name) 'F)
(setf (gethash '66 C-pitch-to-name) 'uF)
(setf (gethash '67 C-pitch-to-name) 'G)
(setf (gethash '68 C-pitch-to-name) 'uG)
(setf (gethash '69 C-pitch-to-name) 'A)
(setf (gethash '70 C-pitch-to-name) 'uA)
(setf (gethash '71 C-pitch-to-name) 'B)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Score to Midi;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tempo time
;(setq tempotime (* (/ 60000 bpm) 8))

;;pitch
; (8 10 12) -> (72 76 79)
(defun score-midi-pitch (alist)
  (if (null alist) ()
      (cons (score-midi-fn (first alist)) (score-midi-pitch (rest alist)))))

(defun score-midi-fn (num)
  (+ (* 12 (nth-value 0 (truncate num 7)))
     (nth (rem num 7) Cm-score-midi-maps)))
;;
(defun score-midi-pitchs (aalist)
  (if (null aalist) ()
      (cons (score-midi-pitch (first aalist))
            (score-midi-pitchs (rest aalist)))))
;(SCORE-MIDI-PITCHS '((8 10 12) (7 8 9 8)))
;-->((72 76 79) (71 72 74 72))

;;duration-tempo 
;a phrases time is tempotime = 4*tempo
;(setq dur-tempo '((1/2 1/4 1/4)(3/8 1/16 1/16 1/2)))

(defun score-midi-durations (tempos tempotime)
  (if (null tempos)()
      (cons (mapcar #' (lambda (x) (* x tempotime))
                    (first tempos))
            (score-midi-durations (rest tempos) tempotime))))

(defun aalist-alist (aalist)
  (if (null aalist)()
      (append (first aalist)
              (make-duration (rest aalist)))))

;;ontime
(defun make-ontime (duration &optional (befortime 0))
  (cons befortime (mapcar #' (lambda (x) (+ x befortime))
                          (reverse (rest (maplist #' (lambda (x)
                                                       (apply #' + x))
                                                  (reverse duration)))))))
          

;;make midi
(defun score-midi-1 (ontime pitch duration channel vol)
  (if (null ontime) ()
      (cons (list (first ontime)
                  (first pitch)
                  (first duration)
                  channel
                  vol)
            (score-midi-1 (rest ontime) (rest pitch) (rest duration) channel vol))))

;(setq pitchs (aalist-alist (SCORE-MIDI-PITCHS '((8 10 12) (7 8 9 8)))))
;(setq durations (aalist-alist (SCORE-MIDI-DURATIONS dur-tempo (* (/ 60000 bpm) 4))))
;(setq ontimes (make-ontime durations))
;(setq channels 1)
;(setq vols 90)

;;use MGC
(load "/Users/bz/Documents/NoU/mgc.lisp")
(setq *WORK-DIR* "/Users/bz/Documents/NoU/")

;(setq *events* (SCORE-MIDI-1 ontimes pitchs durations channels vols))
;(saveit)

;;;dedine a function
;(SCORE-MIDI '((8 10 12) (7 8 9 8)) '((1/2 1/4 1/4)(3/8 1/16 1/16 1/2)) "atest.mid")
;-->/Users/bz/Documents/NoU/atest\\.mid"
(defun score-midi (score-pitch dur-tempo &optional (file-name "scoreplay.mid") (bpm 120) (channels 1) (vols 90))
  (let ((durations (aalist-alist (SCORE-MIDI-DURATIONS dur-tempo (* (/ 60000 bpm) 8))))
        (pitchs (aalist-alist (SCORE-MIDI-PITCHS score-pitch))))
    (let ((ontimes (make-ontime durations)))
      (setq *events* (SCORE-MIDI-1 ontimes pitchs durations channels vols))
      (saveit file-name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          
          
    
        