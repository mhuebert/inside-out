;;
;; # inside-out: a Clojure forms library
;;
;; _status: alpha_
;;
;; The job of a form is to collect some input from a user, so that we can do something with it.
;;
;; The purpose of this library is to provide a simple and efficient way to write forms without
;; error-prone repetition, while avoiding the perils of "bad magic" - abstractions that do too much,
;; and are hard to understand or customize later.
;;
;; Further, we want to enable good user experiences by making it easy to offer clear instructions
;; and feedback to users as they complete a form.
;;
;; The two main ideas behind our approach are the "inside-out" syntax, and "attribute-driven" metadata.
;;

;; Before we begin, we need to set up our namespace. You can mostly ignore this, the important bits are
;; `[inside-out.forms :as forms]` and `[inside-out.reagent :refer [with-form]]`
^#:nextjournal.clerk{:toc? true :no-cache true}
(ns inside-out.notebook
  (:require [inside-out.forms :as forms]
            [inside-out.reagent :refer [with-form]]

   ;; only for notebook purposes
            [clojure.string :as str]
            [inside-out.clerk-ui :as ui :refer [hiccup]]
            [kitchen-async.promise :as p]))

;; ## Syntax Quickstart
;;
;; With `inside-out`, we first describe the "shape" of data you want, and then we get
;; some "primitives" that can be used to build user-input components of any kind.
;;
;; Here we create a `contact-info` form containing a map with the fields `?name` and `?email`:

(with-form [contact-info {:name ?name
                          :email ?email}]
  (reset! ?name "Peter Rabbit")
  @contact-info)

;; Things to note:
;; - `?name` and `?email` are *fields* because they start with a `?`.
;; - Fields behave like atoms, so we can `deref` and `reset!` them.
;; - Fields come from the "insides" of the form, but they are lifted "out"
;;;   into scope so that you can access them individually. Hence the name, inside-out.
;; - When we deref a form, its value is computed using the current values of its fields.

;; How do we build a real form, that takes user input?
;; Let's start with an `:input` element for `?name`:

(hiccup
  (with-form [contact-info {:name ?name
                            :email ?email}]
    [:div
     [:input.border
      {:value @?name
       :on-change (fn [event] (reset! ?name (.. event -target -value)))}]
     [:pre (str @contact-info)]]))

;; Try typing in the box above to see how it works.

;; Using this basic pattern of `@deref` to read the current value, and `reset!` to change it,
;; we can already build any kind of input component, and have the results compose into the
;; data structure we need.
;;
;; A form doesn't need to be a map; it can be any kind of expression at all.
;; For example, a Datomic/DataScript transaction...

(with-form [tx [[:db/add 1 :person/name ?name]]]
  (reset! ?name "Blythe")
  @tx)

;; or an expression:

(with-form [foo (str ?first-name " " ?last-name)]
  @foo)

;; a field can be used more than once in a form:

(with-form [!form [[:db/add 1 :person/pet ?pet-id]
                   [:db/add ?pet-id :pet/name ?pet-name]]]
  (reset! ?pet-id 2)
  (reset! ?pet-name "Fido")
  @!form)


;; Initial values for fields can be provided via an `:init` map, which should map
;; fields to values.

(with-form [foo (str ?first-name " " ?last-name)
            :init {?first-name "Peter"
                   ?last-name "Rabbit"}]
  @foo)

;; Metadata like this can also be provided directly on fields, by wrapping the field
;; in a list and adding key-value pairs:

(with-form [foo (str (?first-name :init "Peter")
                     " "
                     (?last-name :init "Rabbit"))]
  @foo)

;; A field can be used creatively to produce arbitrary output.

(with-form [cars (take ?number (repeat "🚙"))
            :init {?number 3}]
  @cars)

;; Using an HTML slider:

(hiccup
  (with-form [cars (take ?number (repeat "🚙"))
              :init {?number 10}]
    [:<>
     [:input {:type "range" :min "1" :max "40"
              :value @?number
              :on-change (fn [e]
                           (reset! ?number
                                   (js/parseInt (.. e -target -value))))}]
     [:div (str/join @cars)]]))

;; ## Metadata

;; Forms don't only return data. They also communicate with users and validate input. A "field"
;; is not just a piece of data, it's also information about what kind of data is acceptable,
;; where it should come from, what it should be called, and so on. We refer to all this as
;; "metadata" and have a few ways of providing it.

;; 1. Inline metadata is when we wrap a field in a list, with key-value pairs. Eg:
;; `(?name :init "Peter")`
;; 2. A `:meta` option can supply metadata for multiple fields at once, eg
;;  `:meta {?name {:init "Peter"} ?email {:init "rabbit@example.com"}}`
;; 3. The `:init` and `:required` metadata fields have shorthand syntax as they are
;;    so commonly used.

;; `:meta` syntax
(with-form [form {:name ?name :email ?email}
            :meta {?name {:init "Peter"}
                   ?email {:init "rabbit@example.com"}}]
  @form)

;; shorthand syntax for `:required` and `:init`

(with-form [form {:name ?name :email ?email}
            :required [?name ?email]
            :init {?name "Peter"
                   ?email "rabbit@example.com"}]
  @form)

;; ### Attribute metadata

;;
;; Metadata can also be defined for _attributes_ like `:person/name`, which are then
;; inferred for fields based on their position in a data structure:
;; 1. when a field is in the value position in a map, eg. `{<attribute> ?field}`,
;; 2. when a field is in the value position of a :db/add vector:
;;    `[:db/add <id> <attribute> ?field]`

(with-form [!form [[:db/add 1 :person/name ?name]
                   {:pet/id ?pet-id}]]
  [(:attribute ?name)
   (:attribute ?pet-id)])

;; Instead of defining metadata for named fields, we can now add metadata for attributes:

(with-form [!form [[:db/add 1 :person/name ?name]]
            :meta {:person/name {:label "Your name"}}]
  (:label ?name))

;; All fields inherit metadata from the a global var `inside-out.forms/*global-meta*`.
;; This can be useful for attributes that are reused throughout an app.

;; in ClojureScript, we would `set!` the var during app initialization:
'(defn init []
   ;; your app's initialization code
   (set! inside-out.forms/*global-meta* my-attribute-metadata))

;; To show an example here, we'll use `binding` (not recommended for real-world use
;; due to limitations of dynamic vars in ClojureScript):

(binding [forms/*global-meta* {:person/name {:label "Your name"}}]
  (with-form [!form [[:db/add 1 :person/name ?name]]]
    (:label ?name)))

;; We expect attribute-metadata to be most useful for things like labels and validators.

;; ## Component examples

;; Here we define a "managed" text-input view that makes full use of a field's
;; methods and metadata. The forms library supplies handlers for change, blur, and
;; focus events so that this logic can be reused in your own components.

#?(:cljs
   (defn managed-text-input
     "A text-input element that reads metadata from a ?field to display appropriately"
     [?field attrs]
     (let [messages (forms/visible-messages ?field)]
       [:<>
        [ui/input-text-element
         (merge {:placeholder (:label ?field)
                 :value @?field
                 :on-change (forms/change-handler ?field)
                 :on-blur (forms/blur-handler ?field)
                 :on-focus (forms/focus-handler ?field)
                 :class (when (:invalid (forms/types messages))
                          "ring-2 ring-offset-2 ring-red-500 focus:ring-red-500")}
                attrs)]
        (into [:div.mt-3] (map ui/view-message) messages)])))


;; Example with validation. `?name` is no required, so the field is valid until
;; the user starts typing.

(hiccup
  (with-form [form [[:db/add 1 :person/name ?name]]
              :meta {?name {:label "Your full name"
                            :validators [(forms/min-length 3)]}}]

    [:form
     [managed-text-input ?name]
     [:pre (str "valid? " (forms/valid? form))]
     [:pre (str @form)]]))

;; ## Validation

;; Validation is an important concern for forms. Fields may specify a list of `:validators`
;; (it does not matter if these are specified inline or via a registry). Validators return
;; messages, which are read via `(forms/messages ?field)`.

(with-form [!form [[:db/add 1 :person/name ?name]]
            :meta {?name {:validators [:required (forms/min-length 3)]}}]

  (reset! ?name "ma")
  (forms/messages ?name))

;; Validators can depend on other fields in the form. Each validator is a function, which is passed
;; the field's current value, and a map of the other fields in the current form (these fields must be
;; dereferenced to read their values):

(defn validate-child-age [value {:syms [?parent-age]}]
  (when (>= value @?parent-age)
    {:type :invalid
     :content "Child must be younger than parent"}))

(with-form [!form [[:db/add 1 :parent/age ?parent-age]
                   {:db/id 2
                    :child/parent 1
                    :child/age (?child-age :validators [validate-child-age])}]]
  (reset! ?parent-age 10)
  (reset! ?child-age 20)
  (forms/messages ?child-age))


;; A validator can be placed on the form itself by passing it as an option:

(with-form [!form [{:system/id 1
                    :phone/mobile ?mobile
                    :phone/landline ?landline}]
            :validators [(fn [_ {:syms [?mobile ?landline]}]
                           (when-not (or @?mobile @?landline)
                             {:type :invalid
                              :content "At least one phone number must be supplied"}))]]
  ;; form becomes valid after adding a value for ?mobile
  [(forms/valid? !form)
   (do (reset! ?mobile "+49 555 5555555")
       (forms/valid? !form))])

;; A `:required` option may be passed with a list of required fields

(with-form [!form {:name ?name :email ?email :phone ?phone}
            :required [?name ?email ?phone]]
  (forms/messages !form :deep true))

;; Or, include `:required? true` in a field's metadata

(with-form [!form {:name (?name :required? true)
                   :email ?email}
            :meta {?email {:required? true}}]
  (forms/messages !form :deep true))

;; ### Validator functions

;; Validators are functions that returns messages. Fields and forms can both have `:validators`.
;;
;; `(value, context) => [...message]`
;;

;; Each message is a map containing:
;; - `:type`
;;   - `:invalid` implies an invalid value which cannot be persisted.
;;   - `:info` has no effect other than showing content to the user.
;;   - `:in-progress` shows a loading animation.
;; - `:content` (a string, or hiccup)
;; - `:visibility` (`#{:touched :focused :always}`)
;;
;; Examples:

[{:type :info
  :content "Instructions/hints for the user"
  :when #{:always :focused :touched}} ;; default is :touched
 {:type :invalid
  :content "A value that should be rejected"}
 {:type :in-progress
  :content "(optional) - a loading indicator should be displayed while this message is present"}]

;; ## Plural fields

;; A plural field (eg. `:db.cardinality.many`) can be modeled by wrapping a field in a list containing
;; the `?field` symbol for the list, followed by the "template" for each child.
;; Add and remove fields using `forms/add-many!` and `forms/remove-many!`, passing
;; a map of _bindings_ for each child's fields:

(with-form [!form {:features (?features {:name (str/upper-case ?name)
                                         :enabled? ?enabled})}
            :meta {?enabled {:init true}}]

  ;; add two child elements:
  (forms/add-many! ?features {'?name "Paint"}
                   {'?name "Wheels"
                    '?enabled false})

  @?features)

;; Calling `seq` or otherwise iterating over a "plural" field returns a list of its children,
;; whose bindings can be read from the field using their names (quoted symbols). When providing
;; `:initial-children` for a plural field, each child should be a map of bindings as shown below.

(with-form [!form (?features {:name (str/upper-case ?name)})
            :meta {?features {:init [{'?name "Herman"}
                                     {'?name "Sally"}]}}]
  ;; below, we destructure each ?feature using :syms to access its bindings
  (for [{:as ?feature :syms [?name]} ?features]
    @?name))

;; Children are removed by passing a child instance to `forms/remove-many!`.

(with-form [!form (?items {:position ?position})]
  ;; add two items
  (forms/add-many! ?items '{?position 1} '{?position 2})
  ;; remove the first item
  (forms/remove-many! (first ?items))
  ;; Only the second item remains:
  @!form)


;; Example using a :many field:

(hiccup
  (with-form [!form [[:db/add 1 :person/pets
                      ;; define a plural field by adding a :many key to the field.
                      ;; it should contain a "template" for each item in the list.
                      (?pets {:pet/id ?id
                              :pet/name (?name :init "Fido")})]]]

    [:div
     [ui/show-code (str @!form)]

     (doall
      ;; call (seq ?pets) to get a list of fields, which can be destructured using :syms
      ;; to get the child bindings.
      (for [{:as ?pet :syms [?id ?name]} ?pets]
        [:div.flex.items-center.my-2
         {:key @?id}
         [managed-text-input ?name {:placeholder "Name"}]
         [:div.text-red-500.hover:underline.hover:cursor-pointer.mx-3.font-bold
          ;; call forms/remove-many-child! to remove an item
          {:on-click #(forms/remove-many! ?pet)} "X"]]))

     [:div.my-3.text-blue-500.hover:underline.hover:cursor-pointer
      ;; to add an item, call form/add-many-child! with ?pets and a map of bindings,
      ;; using quoted symbols for keys {'?field, value}
      {:on-click #(forms/add-many! ?pets {'?id (rand-int 1000)})} "Add Pet"]]))

;; To specify metadata targeting the children of a plural field, use the :child-meta key:

(with-form [!form {:animal-names (?names ?name)}
            :meta {?names {:child-meta
                           {:validators [(fn [value _]
                                           {:type :info
                                            :content value})]}}}]
  (forms/add-many! ?names '{?name "Toad"}
                   '{?name "Frog"})
  (->> (forms/messages ?names :deep true)
       (map :content)))

;; ## Server submission

;; The `forms/submittable?` and `forms/watch-promise` functions facilitate submission of a form
;; to a remote endpoint.

;; `forms/watch-promise` sets `:loading?` to true, clears any old remote messages, and then waits for
;; the promise to complete. Then `:loading?` is removed, and remote messages are set to the resolved value.
;;
;; The following example includes buttons that show how to handle a successful or failed response.

(hiccup
  (with-form [form {:name (?name :init "Sue")}]
    [:div
     [ui/input-text ?name {}]
     [:pre.text-xs.whitespace-pre-wrap (str @form)]
     (into [:div]
           (map ui/view-message (if (:loading? form)
                                  (forms/wrap-message "Loading...")
                                  (forms/visible-messages form))))
     [:button.bg-blue-500.text-white.p-3.m-3
      {:on-click
       #(forms/watch-promise form
          (p/let [name @?name
                  result (p/timeout 500)]
            (forms/clear! form)
            {:messages (str "Thanks, " name "!")}))}
      "Submit-Success"]
     [:button.bg-red-500.text-white.p-3.m-3
      {:on-click
       #(forms/watch-promise form
          (p/let [name @?name
                  result (p/timeout 500)]
            {:error (str "Sorry " name ", an error occurred.")}))}
      "Submit-Error"]]))

;; `forms/clear!` resets a form to its initial state

(binding [forms/*global-meta* {:a {:init "A"}}]
  (with-form [!form {:a ?a
                     :b (?b :init "B")
                     :c ?c
                     :d (?d [?e (?f :init "F") ?nil]
                            :init [{'?e "E"}])}
              :meta {?c {:init "C"}
                     ?e {:init "E"}}]
    (= @!form
       (do (reset! ?a 1)
           (reset! ?b 2)
           (reset! ?c 3)
           (forms/add-many! ?d '{?e 4 ?f 5 ?nil 6})
           (forms/clear! !form)
           @!form))))

;; ## Functional usage

;; A form can be called as a function, passing a map of bindings, to evaluate its expression:

(def additions (forms/form (+ ?a ?b)))
(additions '{?a 1 ?b 2})

;; ## Implementation notes

;; ### Reagent

;; In ClojureScript, `with-form` uses [Reagent's](http://reagent-project.github.io)  `with-let` macro,
;; so that a form is created once (when a component mounts) and we can use the fields with input components.

;; PRs are welcome for integrations other than Reagent. All that is required is a macro which behaves
;; like `with-let` ("finally" behaviour is not required). See `inside-out.reagent` for macro implementation.

;; # 🙏 NextJournal
;;
;; This library was made at & in collaboration with the folks at NextJournal, who also make [Clerk](https://github.com/nextjournal/clerk),
;; the happy little tool powering this notebook.