(ns reagent.impl.template
  (:require [react :as react]
            [clojure.string :as string]
            [reagent.impl.util :as util :refer [named?]]
            [reagent.impl.component :as comp]
            [reagent.impl.batching :as batch]
            [reagent.impl.input :as input]
            [reagent.impl.protocols :as p]
            [reagent.ratom :as ratom]
            [reagent.debug :refer-macros [dev? warn]]
            [goog.object :as gobj]))

(defonce components (atom {}))

(deftype NativeWrapper [component])

(defn adapt-react-class
  [c]
  (->NativeWrapper c))

;;; Common utilities

(defn ^boolean hiccup-tag? [x]
  (or (named? x)
      (string? x)))

(defn ^boolean valid-tag? [x]
  (or (hiccup-tag? x)
      (ifn? x)
      (instance? NativeWrapper x)))

;;; Props conversion

(def prop-name-cache #js{})

(defn cache-get [o k]
  (when ^boolean (.hasOwnProperty o k)
    (gobj/get o k)))

(defn cached-prop-name [k]
  (if (named? k)
    (if-some [k' (cache-get prop-name-cache (name k))]
      k'
      (let [v (util/dash-to-prop-name k)]
        (gobj/set prop-name-cache (name k) v)
        v))
    k))

(declare convert-prop-value)

(defn kv-conv [o k v]
  (doto o
    (gobj/set (cached-prop-name k) (convert-prop-value v))))

(defn convert-prop-value [x]
  (cond (util/js-val? x) x
        (named? x) (name x)
        (map? x) (reduce-kv kv-conv #js{} x)
        (coll? x) (clj->js x)
        (ifn? x) (fn [& args]
                   (apply x args))
        :else (clj->js x)))

;;; Conversion from Hiccup forms

(defn make-element [this argv component jsprops first-child]
  (case (- (count argv) first-child)
    ;; Optimize cases of zero or one child
    0 (react/createElement component jsprops)

    1 (react/createElement component jsprops
                           (p/as-element this (nth argv first-child nil)))

    (.apply react/createElement nil
            (reduce-kv (fn [a k v]
                         (when (>= k first-child)
                          (.push a (p/as-element this v)))
                         a)
                       #js [component jsprops] argv))))

(defn reag-element [tag v compiler]
  (let [c (comp/as-class tag compiler)
        jsprops #js {}]
    (set! (.-argv jsprops) v)
    (when-some [key (util/react-key-from-vec v)]
      (set! (.-key jsprops) key))
    (react/createElement c jsprops)))

(defn function-element [tag v first-arg compiler]
  (let [jsprops #js {}]
    (set! (.-reagentRender jsprops) tag)
    (set! (.-argv jsprops) (subvec v first-arg))
    ; (set! (.-opts jsprops) opts)
    (when-some [key (util/react-key-from-vec v)]
      (set! (.-key jsprops) key))
    (react/createElement (comp/functional-render-fn compiler tag) jsprops)))

(defn maybe-function-element
  "If given tag is a Class, use it as a class,
  else wrap in Reagent function wrapper."
  [tag v compiler]
  (if (comp/react-class? tag)
    (reag-element tag v compiler)
    (function-element tag v 1 compiler)))

(defn fragment-element [argv compiler]
  (let [props (nth argv 1 nil)
        hasprops (or (nil? props) (map? props))
        jsprops (or (convert-prop-value (if hasprops props))
                    #js {})
        first-child (+ 1 (if hasprops 1 0))]
    (when-some [key (util/react-key-from-vec argv)]
      (set! (.-key jsprops) key))
    (p/make-element compiler argv react/Fragment jsprops first-child)))

(defn native-element [component argv first ^p/Compiler compiler]
  (let [props (nth argv first nil)
        hasprops (or (nil? props) (map? props))
        jsprops (or (convert-prop-value (if hasprops props))
                    #js {})
        first-child (+ first (if hasprops 1 0))]
    (if (and (object? component) (gobj/get component "isInput"))
      (let [input-class (or (.-reagentInput compiler)
                            (let [x (comp/create-class input/input-spec compiler)]
                              (set! (.-reagentInput compiler) x)
                              x))]
        (-> [input-class argv component jsprops first-child compiler]
            (with-meta (meta argv))
            (->> (p/as-element compiler))))
      (do
        (when-some [key (-> (meta argv) util/get-react-key)]
          (set! (.-key jsprops) key))
        (p/make-element compiler argv component jsprops first-child)))))

(defn raw-element [comp argv compiler]
  (let [props (nth argv 2 nil)
        jsprops (or props #js {})]
    (when-some [key (-> (meta argv) util/get-react-key)]
      (set! (.-key jsprops) key))
    (p/make-element compiler argv comp jsprops 3)))

(defn expand-seq [s compiler]
  (into-array (map #(p/as-element compiler %) s)))

(defn expand-seq-dev [s ^clj o compiler]
  (into-array (map (fn [val]
                     (when (and (vector? val)
                                (nil? (util/react-key-from-vec val)))
                       (set! (.-no-key o) true))
                     (p/as-element compiler val))
                   s)))

(defn expand-seq-check [x compiler]
  (let [ctx #js {}
        [res derefed] (ratom/check-derefs #(expand-seq-dev x ctx compiler))]
    (when derefed
      (warn (util/hiccup-err x (comp/comp-name) "Reactive deref not supported in lazy seq, "
                        "it should be wrapped in doall")))
    (when (.-no-key ctx)
      (warn (util/hiccup-err x (comp/comp-name) "Every element in a seq should have a unique :key")))
    res))

(defn vec-to-elem [v compiler fn-to-element]
  (when (nil? compiler)
    (js/console.error "vec-to-elem" (pr-str v)))
  (assert (pos? (count v)) (util/hiccup-err v (comp/comp-name) "Hiccup form should not be empty"))
  (let [tag (nth v 0 nil)]
    (assert (valid-tag? tag) (util/hiccup-err v (comp/comp-name) "Invalid Hiccup form"))
    (case tag
      :> (native-element (nth v 1 nil) v 2 compiler)
      :r> (raw-element (nth v 1 nil) v compiler)
      :f> (function-element (nth v 1 nil) v 2 compiler)
      :<> (fragment-element v compiler)
      (cond
        (get @components tag)
        (native-element (get @components tag) v 1 compiler)

        (instance? NativeWrapper tag)
        (native-element (.-component tag) v 1 compiler)

        :else (fn-to-element tag v compiler)))))

(defn as-element [this x fn-to-element]
  (cond (util/js-val? x) x
        (vector? x) (vec-to-elem x this fn-to-element)
        (seq? x) (if (dev?)
                   (expand-seq-check x this)
                   (expand-seq x this))
        (named? x) (name x)
        (satisfies? IPrintWithWriter x) (pr-str x)
        :else x))

(defn create-compiler [opts]
  (let [id (gensym)
        fn-to-element (if (:function-components opts)
                        maybe-function-element
                        reag-element)]
    (when-let [c (:components opts)]
      (reset! components c))
    (reify p/Compiler
      ;; This is used to as cache key to cache component fns per compiler
      (get-id [this] id)
      (as-element [this x]
        (as-element this x fn-to-element))
      (make-element [this argv component jsprops first-child]
        (make-element this argv component jsprops first-child)))))

(def default-compiler* (create-compiler {}))

(def ^:dynamic default-compiler default-compiler*)

(defn set-default-compiler! [compiler]
  (set! default-compiler compiler))
