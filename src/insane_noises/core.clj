(ns insane-noises.core)
(use 'overtone.live)

;; We use a saw-wave that we defined in the oscillators tutorial

;; Duration (sustain) - 1 second
(definst saw-wave-sec [freq 440 attack 0.01 sustain 1 release 0.1 vol 0.1]
	(* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
		(saw freq)
		vol))

;; Duration (sustain) - 2 seconds
(definst saw-wave-2sec [freq 440 attack 0.01 sustain 2 release 0.1 vol 0.1]
	(* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
		(saw freq)
		vol))


;; Define a function for convenience
(defn note->hz [music-note]
	(midi->hz (note music-note)))

;; Let's make it even easier
;; Saw2 1 second
(defn saw2-sec [music-note]
	(saw-wave-sec (midi->hz (note music-note))))

;; Saw2 2 seconds
(defn saw2-2sec [music-note]
	(saw-wave-2sec (midi->hz (note music-note))))


;; this is one possible implementation of play-chord
(defn play-chord [inst a-chord]
  	(doseq [note a-chord] (inst note)))


;; Kick drum
(def kick (sample (freesound-path 2086)))

;; Hat drum
(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 1 src env)))

;; Weak hat drum
(defn weak-hat []
  (hat 0.3))

;; Setting up a metronome
(def metro (metronome 120))

;; A line of chords
(def line-1 [(chord :E4 :m) (chord :B3 :m) (chord :C4 :M) (chord :D4 :M)])


;; Infinite drums
;; Launches infinite drum beat according to the metronome 'm' starting with the beat number 'beat-num'
(defn metro-beats [m beat-num]
  (at (m (+ 0 beat-num)) (kick))
  (at (m (+ 1 beat-num)) (weak-hat))
  (at (m (+ 2 beat-num)) (kick))
  (at (m (+ 3 beat-num)) (hat))
  (apply-at (m (+ 4 beat-num)) metro-beats m (+ 4 beat-num) [])
  )


;; Sound sequence
;; Launches sound sequence with fixed duration (in quarter beats)
(defn bar-chords [m first-beat duration melody]
  (let [last-beat (count melody)
        later (m (+ (* duration last-beat) first-beat))
        all-beats (map (fn [x] (* x duration)) (range 0 last-beat 1))]
      (map (fn [el i]
             (at (m (+ i first-beat)) (el)))
           melody all-beats)
    )
)

;; Wrapping function
;; Gonna use it later to apply play-chords to a vector of chords
(defn wrapf [f inst chords]
  (map (fn [ch]
         (fn [] (f inst ch)))
       chords))

;; Launching drums and a chord sequence simultaneously
(let [start-point (metro)]
  (metro-beats metro start-point)
  (bar-chords metro start-point 4 (wrapf play-chord saw2-2sec line-1))
)


;; Stop all the music
(stop)
