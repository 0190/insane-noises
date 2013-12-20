(ns insane-noises.core)
(use 'overtone.live)

;; We use a saw-wave that we defined in the oscillators tutorial
(definst saw-wave [freq 440 attack 0.01 sustain 0.5 release 0.1 vol 0.1]
	(* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
		(saw freq)
		vol))

;; Define a function for convenience
(defn note->hz [music-note]
	(midi->hz (note music-note)))

;; Let's make it even easier
(defn saw2 [music-note]
	(saw-wave (midi->hz (note music-note))))

;; this is one possible implementation of play-chord
(defn play-chord [a-chord]
  	(doseq [note a-chord] (saw2 note)))

;; (play-chord (chord :C3 :m))

(def kick (sample (freesound-path 2086)))

;; (kick)

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 1 src env)))

;; (hat)


(def metro (metronome 100))
(def actual-beats [kick hat kick hat])
(def actual-chords [(chord :A4 :m) (chord :D4 :m) (chord :E4 :M) (chord :A3 :m 1)])

(defn crazy-beats [melody]
  (let [last-beat (count melody)
        later (metro (+ last-beat 1))
        all-beats (range 0 last-beat 1)]
      (map (fn [el i]
             (at (metro (+ i 1)) (el)))
           melody all-beats)))

(defn wrapf [f chords]
  (map (fn [ch]
         (fn [] (f ch)))
       chords))

(crazy-beats actual-beats)
(crazy-beats (wrapf play-chord actual-chords))



