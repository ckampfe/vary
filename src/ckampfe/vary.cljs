(ns ^:figwheel-hooks ckampfe.vary
  (:require
   [cljs.js]
   [cljs.reader :as reader]
   [goog.dom :as gdom]
   [re-frame.core :as rf]
   [reagent.core :as r]))

(defn fn-to-data [f]
  (if (not= (first f)
            'defn)
    :error-not-fn
    {:name (second f)
     :args (nth f 2)}))


;; set up eval machinery,
;; thanks to Mike Fikes:
;; https://gist.github.com/mfikes/66a120e18b75b6f4a3ecd0db8a976d84


(let [eval *eval*
      st (cljs.js/empty-state)]
  (set! *eval*
        (fn [form]
          (binding [cljs.env/*compiler* st
                    *ns* (find-ns cljs.analyzer/*cljs-ns*)
                    cljs.js/*eval-fn* cljs.js/js-eval]
            (eval form)))))

;; HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-db [db _]
  {:db {:sliders {}}})

(defn register-arg-handler [arg]
  (rf/reg-sub
   (keyword (str "get-" arg))
   (fn [db]
     (get-in db [:sliders (keyword arg)])))

  (rf/reg-event-fx
   (keyword (str "update-" (name arg)))
   (fn [{:keys [db] :as coeffects} [_ change]]
     (let [new-db (-> db (assoc-in [:sliders (keyword arg)] (js/parseInt change)))
           eval-result (when (:evaled-fn new-db)
                         (let [arg-keywords (map keyword (-> new-db
                                                             :name-and-args
                                                             :args))
                               arg-vals (map (fn [kw]
                                               (get-in new-db [:sliders kw]))
                                             arg-keywords)]
                           (eval (apply (:evaled-fn new-db) arg-vals))))]

       {:db (assoc-in new-db
                      [:fn-result]
                      eval-result)}))))

(defn update-input-fn [{:keys [db] :as coeffects} [_ change]]
  (let [read-input-fn (try (reader/read-string change)
                           (catch js/Error e
                             :not-fn))
        name-and-args (when-not (= read-input-fn :not-fn)
                        (fn-to-data read-input-fn))
        evaled-fn (when-not (= read-input-fn :not-fn)
                    (eval read-input-fn))

        new-db {:db (assoc db
                           :input-fn change
                           :read-input-fn read-input-fn
                           :name-and-args name-and-args)}]

    (when (not= read-input-fn :not-fn)
      (doseq [arg (:args name-and-args)]
        (register-arg-handler arg)))

    (let [new-db (reduce (fn [acc arg]
                           (assoc-in acc [:db :sliders (keyword arg)] 0))
                         new-db
                         (-> new-db
                             :db
                             :name-and-args
                             :args))

          fn-result (when evaled-fn
                      (let [arg-keywords (map keyword (-> new-db
                                                          :db
                                                          :name-and-args
                                                          :args))
                            arg-vals (map (fn [kw]
                                            (get-in new-db [:db :sliders kw]))
                                          arg-keywords)]
                        (eval (apply evaled-fn arg-vals))))]

      (-> new-db
          (assoc-in [:db :fn-result] fn-result)
          (assoc-in [:db :evaled-fn] evaled-fn)))))

;; HANDLER REGISTRATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rf/reg-event-fx
 :initialize-db
 initialize-db)

(rf/reg-event-fx
 :update-input-fn
 update-input-fn)

;; DATA SUBSCRIPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn input-fn [db]
  (:input-fn db))

(rf/reg-sub
 :input-fn
 input-fn)

(defn args [db]
  (if-let [argss (:args (:name-and-args db))]
    argss
    []))

(rf/reg-sub
 :args
 args)

(rf/reg-sub
 :fn-name
 (fn [db] (-> db :name-and-args :name)))

(rf/reg-sub
 :fn-result
 (fn [db] (:fn-result db)))


;; COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn slider [arg-name]
  ^{:key arg-name} [:input {:type "range" :min 0 :max 100
                            :style {:width "65%"}
                            :value @(rf/subscribe [(keyword (str "get-" arg-name))])
                            :on-change #(rf/dispatch [(keyword (str "update-" (name arg-name)))
                                                      (-> % .-target .-value)])}])

(defn input-fn-el []
  [:div {:class "col-lg"}
   [:textarea {:type "text"
               :class "form-control"
               :rows "15"
               :value @(rf/subscribe [:input-fn])
               :on-change #(rf/dispatch [:update-input-fn (-> % .-target .-value)])}]])

(defn sliders []
  [:div {:class "col-lg"}
   (doall (for [arg @(rf/subscribe [:args])]
            ^{:key arg} [:div
                         (str "value for " arg " is: " @(rf/subscribe [(keyword (str "get-" arg))]))
                         [:div (slider arg)]]))
   (when-let [fn-name @(rf/subscribe [:fn-name])]
     [:h3
      (str "result of " fn-name
           " is: " @(rf/subscribe [:fn-result]))])])

(defn columns [left right]
  [:div {:class "row"}
   [left]
   [right]])

(defn app []
  [:div
   [:h1 "Vary"]
   [columns
    input-fn-el
    sliders]])

(defn ^:after-load init []
  (rf/dispatch-sync [:initialize-db])
  (r/render-component [app]
                      (.getElementById js/document "app")))

(init)
