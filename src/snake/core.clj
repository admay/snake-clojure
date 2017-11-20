(ns snake.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener KeyEvent)))

;; Functional model
(def width 50)
(def height 30)
(def point-size 15)
(def turn-millis 100)
(def win-length (* width height))
(def dirs {KeyEvent/VK_LEFT  [-1  0]
           KeyEvent/VK_RIGHT [ 1  0]
           KeyEvent/VK_UP    [ 0 -1]
           KeyEvent/VK_DOWN  [ 0  1]})

(defn create-apple []
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple})

(defn create-snake []
  {:body (list [3 1] [2 1] [1 1] [0 1])
   :dir [1 0]
   :type :snake
   :color (Color. 15 160 70)})

(defn point-to-screen-rect
  "Returns the coords for the top left corner of
  the rect. and the height and width"
  [[x y]]
  [(* x point-size) (* y point-size) point-size point-size])

(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body
         (cons
          (let [[head-x head-y] (first body)
                [dir-x dir-y] dir]
            [(+ head-x dir-x) (+ head-y dir-y)])
          (if grow
            body ; adds the new point to the head of the snake
            (butlast body)))))

(defn turn [snake newdir]
  (assoc snake :dir newdir))

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? [head body]
  (contains? (set body) head))

(defn head-outside-bounds? [[x y]]
  (or
   (> x width)
   (< x 0)
   (> y height)
   (< y 0)))

(defn lose?
  "Takes a snake map as a param and determines
  if you lose the game."
  [{[head & body] :body}]
  (or (head-overlaps-body? head body)
      (head-outside-bounds? head)))

(defn eats?
  "Takes a snake as a param and determines
  if the snake will eat."
  [{[head] :body} {apple :location}]
  (= head apple))


;; Mutable model
(defn update-positions [snake apple]
  (dosync
   (if (eats? @snake @apple)
     (do (ref-set apple (create-apple))
         (alter snake move :grow))
     (alter snake move)))
  nil)

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

(defn reset-game [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)


;; GUI
(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defn game-panel [frame snake apple]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e]
      (update-positions snake apple)
      (when (lose? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e]
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]
    (.setFocusable panel true)
    (.addKeyListener panel panel)
    (.add frame panel)
    (.pack frame)
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.setVisible frame true)
    (.start timer)))

(defn -main
  "Start the game"
  [& args]
  (game))
