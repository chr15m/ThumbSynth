(ns thumbsynth.main
  (:require
    [applied-science.js-interop :as j]
    [shadow.resource :as rc]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [alandipert.storage-atom :refer [local-storage]]
    ["react-piano" :refer [Piano]]
    ["@tonaljs/midi"
     :refer [midiToNoteName]
     :rename {midiToNoteName note-name}]
    ["@tonaljs/scale" :as scale]
    ["@tonaljs/note" :refer [midi freq]]
    ["virtual-audio-graph"
     :refer [gain oscillator biquadFilter default]
     :rename {default create-graph}]
    [dopeloop.main :refer [audio-context
                           seamless-loop-audio-buffer!
                           stop-source!
                           manage-audio-context-ios
                           poll-device-volume on-ios?
                           lock-screen-orientation
                           on-device-ready]]))

(def ios (on-ios?))
(def bpm-min 60)
(def bpm-max 240)
(def scales (scale/names))

(def music-keyboard-map {:black [1 3 nil 7 9 11 nil]
                         :white [0 2 4 6 8 10 12]})

(def initial-state {:bpm 90 ; persisted
                    :swing 0 ; persisted
                    :playing false
                    :device-volume 1
                    :audio {:context nil
                            :source nil
                            :buffer nil}
                    :notes {:scale-name "major"
                            :root "C"}
                    :taps []
                    :show-menu false
                    :playing-notes {}})

(def local-storage-keys [:bpm :swing])

(defonce state (local-storage (r/atom initial-state)
                              :thumbsynth-settings
                              (fn [*state]
                                (select-keys *state local-storage-keys))
                              (fn [*state]
                                (merge initial-state *state))))

(def buttons {:play (rc/inline "sprites/button-play.svg")
              :stop (rc/inline "sprites/button-stop.svg")
              :bars (rc/inline "sprites/bars.svg")
              :exex (rc/inline "sprites/times.svg")
              :loop (rc/inline "sprites/refresh.svg")
              :thumb (rc/inline "sprites/thumbs-up.svg")
              :metronome (rc/inline "sprites/metronome.svg")})

(defn create-new-context [*state]
  (assoc *state :context (audio-context.)))

(defn make-graph [graph {:keys [note cutoff-note wave-form rez on]}]
  (let [t (j/get graph :currentTime)
        gain-value (or
                     (j/get-in graph
                               [ :virtualNodes 0 :audioNode :gain :value])
                     0)]
    (j/lit
      {0 (gain "output"
               (if on
                 (j/lit {:gain
                         [["setValueAtTime" gain-value t]
                          ["linearRampToValueAtTime" 0.5 (+ t 0.1)]]})
                 (j/lit {:gain
                         [["setValueAtTime" gain-value t]
                          ["setTargetAtTime" 0 t 0.05]]})))
       1 (biquadFilter 0 #js {:type "lowpass"
                              :Q rez
                              :gain rez
                              :frequency (freq cutoff-note)})
       2 (oscillator 1 #js {:type (if (= wave-form "0") "square" "sawtooth")
                            :frequency (freq note)})})))

(defn update-audio-from-state [graph old-audio-params new-audio-params]
  (when (not= old-audio-params new-audio-params)
    (.update graph
             (if new-audio-params
               (make-graph graph new-audio-params)
               #js {}))))

(defn make-click-track-audio-buffer [{:keys [context bpm swing] :as *state}]
  (let [beat-seconds (/ (/ 60 bpm) 2)
        beats 2
        sample-rate (j/get context :sampleRate)
        frames-per-beat (int (* beat-seconds sample-rate))
        frame-count (* beats frames-per-beat)
        swing-frames (-> swing (/ 100) (* frames-per-beat))
        buffer (.createBuffer context 2 frame-count sample-rate)]
    (doseq [b [0]]
      (let [array-buffer (.getChannelData buffer b)]
        (doseq [beat (range beats) i (range frames-per-beat)]
          (aset array-buffer
                (+ i (* beat frames-per-beat))
                (if
                  (if (= (mod beat 2) 0)
                    (< i 882)
                    (and (< (- i swing-frames) 882)
                         (> i swing-frames)))
                  1.0
                  -0.01)))))
    (assoc *state :audio-buffer buffer)))

(defn play-click-track! [{:keys [context audio-buffer audio-source] :as *state}]
  (assoc *state :audio-source
         (seamless-loop-audio-buffer! context audio-buffer audio-source)))

(defn update-loop! [state]
  (let [{:keys [playing]} @state]
    (when playing
      (swap! state
             #(-> % make-click-track-audio-buffer play-click-track!)))))

(defn play! [state]
  (swap! state #(-> %
                    (assoc :playing true)
                    (assoc-in [:audio :context] (audio-context.))))
  (update-loop! state))

(defn stop! [state]
  (let [click-track-audio-source (@state :audio-source)]
    (.close (-> @state :audio :context))
    (swap! state #(-> %
                      (dissoc :playing)
                      (update-in [:audio] dissoc
                                 :audio-source :audio-buffer :context)))
    (stop-source! click-track-audio-source)))

(defn event-pos-to-note-params [midi-notes ev]
  (let [el (j/get ev :currentTarget)
        box (.getBoundingClientRect el)
        l (j/get box :left)
        t (j/get box :top)
        w (- (j/get box :right) l)
        h (- (j/get box :bottom) t)
        x (j/get ev :clientX)
        y (j/get ev :clientY)
        horizontal (-> x (- l) (/ w))
        vertical (-> y (- t) (/ h))
        octave (* horizontal 5)
        note-index (int (* (- octave (int octave)) (count midi-notes)))
        note (nth midi-notes note-index)]
    {:note (note-name (+ note (+ (* (int octave) 12) -36)))
     :cutoff-note (-> (- 1 vertical) (* 96) (+ 32) note-name)}))

(defn update-oscillator [*state note-params]
  (update-in *state [:audio-params] merge note-params {:on true}))

(defn stop-oscillator [*state _ev]
  (assoc-in *state [:audio-params :on] false))

(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn new-tap [*state]
  (let [taps (-> *state :taps)
        bpm (-> *state :bpm)
        taps (if (seq? taps) taps [])
        now (.getTime (js/Date.))
        taps (conj (if (seq? taps) taps []) now)
        taps (filter #(> % (- now 3000)) taps)
        tap-threshold (> (count taps) 3)
        tap-diffs (reduce
                    (fn [[last-tap accum] tap]
                      [tap
                       (if (= last-tap tap)
                         accum
                         (conj accum (- last-tap tap)))])
                    [now []]
                    taps)
        avg-tap-length (average (second tap-diffs))
        bpm (if tap-threshold
              (int (/ 60000 avg-tap-length))
              bpm)]
    (assoc *state :bpm bpm :taps taps)))

(defn tap! [state]
  (let [previous-state @state
        updated-state (swap! state new-tap)]
    (when (not= updated-state previous-state)
      (update-loop! state))))

(defn update-val! [state k ev]
  (swap! state
         assoc-in k (if (number? ev) ev (int (-> ev .-target .-value)))))

(defn set-bpm! [state v]
  (update-val! state [:bpm] v)
  (update-loop! state))

(defn get-bpm [*state]
  (-> *state :bpm int (min bpm-max) (max bpm-min)))

(defn get-swing [*state]
  (-> *state :swing int (min 100) (max 0)))

(defn component-icon [svg]
  [:span.icon {:ref (fn [el] (when el (aset el "innerHTML" svg)))}])

(defn component-slider [k value min-val max-val]
  (let [midpoint (/ (+ min-val max-val) 2)]
    [:label
     [:span (when (< value midpoint) {:class "right"}) (name k)]
     [:input
      {:type "range"
       :min min-val
       :max max-val
       :on-change #(update-val! state [k] %)
       :on-mouse-up #(update-loop! state)
       :on-touch-end #(update-loop! state)
       :value value}]]))

(defn component-menu-toggle [state]
  [:div#menu.input-group
   [:span {:on-click #(swap! state update :show-menu not)}
    [component-icon (if (:show-menu @state) (:exex buttons) (:bars buttons))]]])

(defn component-help [state]
  [:div#help
   [:div
    [component-menu-toggle state]
    [:div
     [:p [:a {:href "https://dopeloop.ai" :target "_BLANK"}
          [:button "Get more apps"]]]
     [:h3 "Help"]
     [:p "Use this app with a pocket operator device."]
     [:p "Plug a 3.5mm stereo cable from this device into the left input
         of your pocket operator.
         The sync signal is sent over the audio cable."]
     [:p "Turn the volume all the way up on this device."]
     [:p "Set sync mode on your pocket operator to SY4 or SY5
         (sync pass-through) using the top-right button + BPM."]
     [:p "Press play to start the sync signal."]
     (when (not ios)
       [:<>
        [:h3 "Tips"]
        [:p "Huawei phones and maybe others have a 'battery saving' mode
            which lowers the headphone level.
            This prevents sync from working. You can turn off this mode
            in your phone settings."]])
     [:h3 "Privacy"]
     [:p
      "The app does not access, collect, use,
      or share any of your personal data.
      We don't collect any personal information."]
     [:p [:a {:href "https://dopeloop.ai/app-privacy-policy.html"}
          "Full privacy policy"] "."]
     [:p [:button.ok {:on-click #(swap! state update :show-menu not)} "Ok"]]]]
   [:div]])

(defn compute-scale [*state]
  (let [scale-name (-> *state :notes :scale-name)
        scale-root (-> *state :notes :root)
        scale (scale/get (str scale-root " " scale-name))]
    scale))

(defn component-main [state]
  (let [playing (:playing @state)
        device-volume (:device-volume @state)
        scale (compute-scale @state)
        midi-notes (map #(midi (str % "4")) (j/get scale :notes))]
    [:div#app
     [:div
      [component-menu-toggle state]
      [:div.input-group
       (let [wave-form (-> @state :audio-params :wave-form)]
         [:label
          (if (= wave-form "0")
            [:span {:class "right"} "sqr"]
            [:span "saw"])
          [:input {:type "range" :min 0 :max 1 :step 1
                   :value wave-form
                   :on-change #(swap! state assoc-in [:audio-params :wave-form] (-> % .-target .-value))}]])
       (let [rez (-> @state :audio-params :rez)]
         [:label
          [:span (when (< rez 10) {:class "right"}) "rez"]
          [:input {:type "range" :min 0 :max 20 :step 1
                   :value rez
                   :on-change #(swap! state assoc-in [:audio-params :rez] (-> % .-target .-value))}]])]
      [:div.input-group
       [:select {:name "root-note"
                 :value (-> @state :notes :root)
                 :on-change
                 (fn [ev]
                   (swap! state assoc-in [:notes :root]
                          (-> ev .-target .-value)))}
        (doall
          (for [l (map (fn [n] (note-name n #js {:sharps true
                                                 :pitchClass true}))
                       (range 0 12))]
            [:option {:key l} l]))]
       [:select {:name "scale"
                 :value (-> @state :notes :scale-name)
                 :on-change
                 (fn [ev]
                   (swap! state assoc-in [:notes :scale-name]
                          (-> ev .-target .-value)))}
        (doall
          (for [l scales]
            [:option {:key l} l]))]]
      [:div.input-group
       [:div.keyboard-container {:style {:pointer-events "none"}}
        [:> Piano {:activeNotes midi-notes
                   :noteRange #js {:first 60 :last 71}
                   :useTouchEvents true
                   :playNote (fn [midiNumber]
                               (js/console.log "down" midiNumber)
                               #_ (swap! state assoc-in [:playing-notes
                                                         midiNumber]))
                   :stopNote (fn [midiNumber]
                               (js/console.log "up" midiNumber)
                               #_ (swap! state update-in [:playing-notes]
                                         dissoc midiNumber))}]]]
      [:div.input-group
       [:div.touchpad
        {:on-pointer-down #(swap! state update-oscillator
                                  (event-pos-to-note-params midi-notes %))
         :on-pointer-move #(when (or (> (j/get % :pressure) 0) ios)
                             (swap! state update-oscillator
                                    (event-pos-to-note-params midi-notes %)))
         :on-pointer-leave #(swap! state stop-oscillator %)
         :on-pointer-out #(swap! state stop-oscillator %)
         :on-pointer-up #(swap! state stop-oscillator %)}
        [component-icon (:thumb buttons)]]]
      [:label
       [:span #_ {:class "right"} "vol"]
       [:input {:type "range" :min 0 :max 1 :step 0.01}]]
      [:div.input-group
       [:div.highlight.device-warning
        (when (< device-volume 0.9)
          "Set device volume to max for sync.")]
       [:span.rounded [component-icon (:metronome buttons)]]
       [:button.rounded [component-icon (:loop buttons)]]
       [:button.rounded {:on-click #(if playing (stop! state) (play! state))}
        [component-icon (if playing
                          (:stop buttons)
                          (:play buttons))]]]]]))

(defn component-pages [state]
  (if (:show-menu @state)
    [component-help state]
    [component-main state]))

(defn reload! {:dev/after-load true} []
  (rdom/render [component-pages state]
               (-> js/document (.querySelector "main"))))

(defn main! []
  (manage-audio-context-ios #(-> @state :audio :context))
  (poll-device-volume 250 #(swap! state assoc :device-volume %))
  (on-device-ready #(lock-screen-orientation "portrait-primary"))
  (let [audio-graph (create-graph)]
    (add-watch state :audio-watcher
               (fn [_k _a old-state new-state]
                 (update-audio-from-state audio-graph
                                          (:audio-params old-state)
                                          (:audio-params new-state)))))
  (reload!))
