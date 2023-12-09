(ns app.ui
  (:require
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom :refer [div input p button ul li h1]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    ))


(defmutation delete-todo [{:keys [id list-id]}]
  (action [{:keys [state]}]
          (swap! state update :todo-item/id dissoc id)
          (swap! state merge/remove-ident* [:todo-item/id id] [:todo-list/id list-id :todo-list/items])))

(defmutation edit-todo [{:keys [id new-value]}]
  (action [{:keys [state]}]
          (swap! state assoc-in [:todo-item/id id :todo-item/value] new-value)))

(defsc TodoItem [this {:todo-item/keys [id value list-id]}]
  {:query          [:todo-item/id
                    :todo-item/value
                    :todo-item/list-id]
   :ident          :todo-item/id
   :initLocalState (fn [this {:todo-item/keys [id value list-id]}]
                     {:editing?       false
                      :edit-value     value
                      :on-edit-click        #(comp/set-state! this (assoc (comp/get-state this) :editing? true))
                      :on-edit-change (fn [e]
                                        (comp/set-state! this (assoc (comp/get-state this) :edit-value (.. e -target -value))))
                      :on-edit-ok     (fn [_]
                                        (comp/transact! this [(edit-todo {:id id :new-value (comp/get-state this :edit-value)})])
                                        (comp/set-state! this (assoc (comp/get-state this) :editing? false)))
                      :on-edit-cancel (fn [_]
                                        (comp/set-state! this (assoc (comp/get-state this) :editing? false
                                                                                           :edit-value value)))
                      :on-delete      #(comp/transact! this [(delete-todo {:id id :list-id list-id})])})}
  (let [{:keys [on-delete on-edit-click editing? edit-value on-edit-change on-edit-ok on-edit-cancel]} (comp/get-state this)]
    (if editing?
          (ul  :.list-group.list-group-flush
           (li :.list-group-item(input {:type     "text"
                  :value    edit-value
                  :onChange on-edit-change})
          (button :.btn.btn-primary {:style {:margin "5px"} :type "button" :onClick on-edit-ok} "OK")

          (button :.btn.btn-primary {:style {:margin "5px"} :type "button" :onClick on-edit-cancel} "Cancel")))
          (ul :.list-group.list-group-flush
          (li :.list-group-item {}
          value
          (button :.btn.btn-primary {:style {:margin "5px"} :type "button" :onClick on-edit-click} "Edit")
              (button :.btn.btn-primary {:style {:margin "5px"} :type "button" :onClick on-delete} "Delete"))))))





(def ui-todo-item (comp/factory TodoItem {:keyfn :todo-item/id}))

(defmutation add-todo [{:keys [list-id value]}]
  (action
    [{:keys [state]}]
    (let [new-id (inc (get-in @state [:todo-list/id list-id :todo-list/item-count]))]
      (swap! state merge/merge-component TodoItem {:todo-item/id      new-id
                                                   :todo-item/value   value
                                                   :todo-item/list-id list-id}
             :append [:todo-list/id list-id :todo-list/items])
      (swap! state update-in [:todo-list/id list-id :todo-list/item-count] inc))))

(defsc TodoList [this {:todo-list/keys [items]}]
  {:query         [:todo-list/id
                   :todo-list/item-count
                   {:todo-list/items (comp/get-query TodoItem)}]
   :ident         :todo-list/id
   :initial-state {:todo-list/item-count 0
                   :todo-list/id         :param/id
                   :todo-list/items      []}}
  (when (not-empty items)
    (ul (map ui-todo-item items))))

(def ui-todo-list (comp/factory TodoList))

(defsc TodoInput [this {:todo-input/keys [value list]}]
  {:query          [:todo-input/id
                    :todo-input/value
                    {:todo-input/list (comp/get-query TodoList)}]
   :ident          :todo-input/id
   :initial-state  {:todo-input/id    :param/id
                    :todo-input/value ""
                    :todo-input/list  {:id :param/list-id}}
   :initLocalState (fn [this {:todo-input/keys [value list]}]
                     {:input-value value
                      :on-change   (fn [e]
                                     (comp/set-state! this (assoc (comp/get-state this) :input-value (.. e -target -value))))
                      :on-add      (fn [e]
                                     (.preventDefault e)
                                     (comp/transact! this [(add-todo {:list-id (:todo-list/id list)
                                                                      :value   (comp/get-state this :input-value)})])
                                     (comp/set-state! this (assoc (comp/get-state this) :input-value "")))})}
  (let [{:keys [on-change on-add input-value]} (comp/get-state this)]
    (div
      (h1 "To do list") (input :.form-control
                 {:type     "text"
                  :placeholder "Item"
                 :value    input-value
                 :onChange on-change})
         (button :.btn.btn-primary {:style {:margin "3px"} :type "button" :onClick on-add} "New item"))))

(def ui-todo-input (comp/factory TodoInput))

(defsc Root [this {:root/keys [todo-list todo-input]}]
  {:query         [{:root/todo-list (comp/get-query TodoList)}
                   {:root/todo-input (comp/get-query TodoInput)}]
   :initial-state {:root/todo-input {:id 1 :list-id 1}
                   :root/todo-list  {:id 1}}}
  (div (ui-todo-input todo-input)
       (ui-todo-list todo-list)))




