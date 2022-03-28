;; # Inside-Out
;;
;; a Clojure forms library _(alpha - [feedback welcome](https://github.com/mhuebert/inside-out/discussions))_
;;
;; This library came out of work at NextJournal, looking for a better "forms" abstraction. We were annoyed
;; at how often form-generation code promises "magic" and then delivers... well, too much of it.
;; Originally intended as a tightly-coupled helper attached to a client triple-store, then stumbled into a
;; more general & pleasing model.
;;
;; ## Features
;;
;; 1. Efficient syntax for defining a form and its fields in one step. Each field stores a value,
;;    and is used to create input components using `deref` and `reset!`. The form's "output" can take
;;    any shape.
;;
;; 2. A metadata system that eliminates boilerplate, encourages data-driven design, and handles
;;    common concerns like validation, hints, and error messages.
;;
;; Explicit design goal: avoid "bad magic", abstractions that do too much and become hard to
;; understand or customize later.
;;
;; ## Namespace Setup
;;
;; You can mostly ignore this step. Only take note of
;; `[inside-out.forms :as forms]` and `[inside-out.reagent :refer [with-form]]`.

^:nextjournal.clerk/toc?
(ns ^:nextjournal.clerk/no-cache inside-out.notebook
  (:require [inside-out.forms :as forms]
            [inside-out.reagent :refer [with-form]]

   ;; only for notebook purposes
            [clojure.string :as str]
            [inside-out.clerk-ui :as ui :refer [cljs]]
            [kitchen-async.promise :as p]))

;; ## Quick Example

(with-form [contact-info {:name ?name}]
  (reset! ?name "Peter Rabbit")
  @contact-info)

;; What do we see here?
;;
;; 1. `with-form` creates a new "form" called `contact-info`.
;; 1. The shape of the form is `{:name ?name}`. This is what we "get back" when we deref the form,
;;    eg. `@contact-info`.
;; 1. The form has one field, `?name`. Every symbol that starts with `?` will become a field.
;;    We `@deref` and `reset!` fields to read/write them.
;;
;; It's time for a real form. Let's add an `:input` component, and show the form's contents.

(cljs
 (with-form [contact-info {:person/name ?name}]
   [:div
    [:input.border.p-3
     {:value @?name
      :on-change (fn [event] (reset! ?name (.. event -target -value)))
      :placeholder "Your name:"}]
    [:pre (str @contact-info)]]))

;; Type into the input and see how the form's value updates.

;; Fields like `?name` come from the "inside" of the form, but are lifted "out" into scope
;; so that you can use them in your UI. This is where the "Inside-Out" name comes from.

;; In ClojureScript, `with-form` uses Reagent's `with-let` so that our form survives re-render.

;; A form can take any shape, and the same field can be used more than once.

(with-form [!form [[:db/add 1 :person/pet ?pet-id]
                   [:db/add ?pet-id :pet/name ?pet-name]]]
  (reset! ?pet-id 2)
  (reset! ?pet-name "Fido")
  @!form)

;; Initial field values can be supplied via `:init` metadata on a field. We can add
;; "inline" metadata by wrapping the field in a list, and adding key-value pairs after it:

(with-form [contact-info {:name (?name :init "Peter")}]
  @contact-info)

;; Or, add metadata keys as options after the form. Each key should contain a map of fields
;; to values.

(with-form [foo (str ?first-name " " ?last-name)
            :init {?first-name "Peter"
                   ?last-name "Rabbit"}]
  @foo)

;; Separating the form from field metadata can help keep the form's structure clean and readable.

;; ## A form's shape
;;
;; A form can be any expression, so fields do not have to map 1:1 to "locations" in
;; the form.

(with-form [cars (take ?number (repeat "🚙"))
            :init {?number 3}]
  @cars)

;; Interactive example:

(cljs
 (with-form [cars (take ?number (repeat "🚙"))
             :init {?number 3}]
   [:div
    [:input {:type "range" :min "1" :max "10"
             :value @?number
             :on-change (fn [e] (->> e .-target .-value js/parseInt (reset! ?number)))}]
    (str " " @?number " ")
    (str/join @cars)]))

;; This is quite different from giving each field a "path" or "cursor" into an atom,
;; another common approach to making forms, which requires that the "structure" of the form
;; is static.

;; ## Metadata

;; A "field" is not just a piece of data, it's also information about the data itself _(is it valid?)_
;; and how it should be represented to a person _(what is it called?)_. We call this "metadata" and
;; have a few ways of providing it.

;; 1. Inline, by wrapping the field in a list: `(?name :init "Peter")`
;; 2. Adding metadata keys after the form.
;;    The value can be a map of `{?field <value>}` or a vector of `[?field1, ?field2]` which sets
;;    each value to `true`.
;; 3. Adding a `:meta` option, of the shape `{?field {:meta-key <meta-value}}`
;;
;; Examples:

(with-form [form {:name ?name
                  :email ?email}
            :init {?name "Peter"
                   ?email "Rabbit"}
            :required [?email]] ;; equivalent to {?email true}
  (:required ?email))

;; We can read metadata by looking up a key on a field, as seen above in `(:required ?email)`.

;; ### Attribute Metadata

;; Let us be data-driven!
;;
;; Metadata can be defined for attributes like `:person/name`, which are _inferred_
;; for fields based on their position in a data structure:
;; 1. when a field is in the value position in a map, eg. `{<attribute> ?field}`,
;; 2. when a field is in the value position of a `:db/add` vector:
;;    `[:db/add <id> <attribute> ?field]`

(with-form [!form {:pet/id ?pet-id}]
  ;; attribute is inferred from key in a map
  (:attribute ?pet-id))

(with-form [!form [[:db/add 1 :person/name ?name]]]
  ;; attribute is inferred based on position in a :db/add vector
  (:attribute ?name))

;; Defining a map of attribute-metadata:

(def app-attributes {:person/name {:label "Your name"}
                     :person/phone {:validators [(fn [value _]
                                                   ;; ensure value is a phone number
                                                   )]}})

;; We can pass that in as a :meta option:

(with-form [!form [[:db/add 1 :person/name ?name]]
            :meta app-attributes]
  (:label ?name))

;; In practice, you'll probably want to define attribute metadata once in your app and re-use it
;; everywhere. To support this use-case, fields inherit from a globally defined var
;; `inside-out.forms/global-meta`.

;; in ClojureScript, we would `set!` the var during app initialization:
(cljs

 ;; inside your app's initialization code
 (set! inside-out.forms/global-meta {:person/name {:field/label "Your name"}})

 [:pre (str inside-out.forms/global-meta)])

;; Now it is available globally:

(cljs
 (with-form [!form [[:db/add 1 :person/name ?name]]]
   (:field/label ?name)))

;; ## Validation

;; Fields may specify a list of `:validators` are called on-demand when we read
;; a field's "messages" via `(forms/messages ?field)`.

(with-form [!form [[:db/add 1 :person/name ?name]]
            :meta {?name {:validators [(forms/min-length 3)]}}]

  (reset! ?name "ma")
  (forms/messages ?name))

;; Each validator is a function which is passed the field's current value and a map of
;; the other fields in the form (which must be dereferenced to read their values):

(defn validate-child-age [value {:syms [?parent-age]}]
  (when (>= value @?parent-age)
    {:type :invalid
     :content "Child must be younger than parent"}))

(with-form [!form [{:db/add 1
                    :parent/age ?parent-age}
                   {:db/id 2
                    :child/parent 1
                    :child/age (?child-age :validators [validate-child-age])}]]
  (reset! ?parent-age 10)
  (reset! ?child-age 20)
  (forms/messages ?child-age))


;; Validators for the form itself can be passed using a :form/validators option

(with-form [!form {:system/id 1
                   :phone/mobile ?mobile
                   :phone/landline ?landline}
            :form/validators [(fn [_ {:syms [?mobile ?landline]}]
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

;; Or, include `:required true` in a field's metadata

(with-form [!form {:name (?name :required true)
                   :email ?email}
            :meta {?email {:required true}}]
  (forms/messages !form :deep true))

;; ### Validator functions

;; A validator is a function with the signature `(value, context) => [...message]`
;;
;; A message is a map containing:
;; - `:type`
;;   - `:invalid` implies an invalid value which cannot be persisted.
;;   - `:hint` will show when a field is focused.
;; - `:content` (a string, or hiccup)
;; - `:visibility` (`#{:touched :focused :always}`)
;;
;; Examples:

[{:type :hint
  :content "Instructions/hints for the user"
  :when #{:always :focused :touched}} ;; default is :focused
 {:type :invalid
  :content "A value that should be rejected"}]

;; ## Components
;;
;; Helper functions are provided for managing a component's blur and focus events to track
;; the "touched" status of a field, useful for limiting visibility of validation messages.

;; A field is considered "touched" if a user has already interacted with it, or if its parent
;; form is touched (we "touch" a form itself when a user tries to submit it).

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

;; Example with validation. `?name` is not required, so the field is valid until
;; the user starts typing.

(cljs
 (with-form [form [[:db/add 1 :person/name ?name]]
             :meta {?name {:label "Your full name"
                           :validators [(forms/min-length 3)]}}]

   [:form
    [managed-text-input ?name]
    [:pre (str "valid? " (forms/valid? form))]
    [:pre (str @form)]]))

;; ## Plural fields

;; A plural field (eg. `:db.cardinality.many`) can be modeled by wrapping a field in a list containing
;; the `?field` symbol for the list, followed by the "template" for each child.
;; Add and remove fields using `forms/add-many!` and `forms/remove-many!`, passing
;; a map of _bindings_ for each child's fields:

(with-form [!form {:features (?features :many
                                        {:name (str/upper-case ?name)
                                         :enabled? ?enabled})}
            :meta {?enabled {:init true}}]

  ;; add two child elements:
  (forms/add-many! ?features {'?name "Paint"}
                   {'?name "Wheels"
                    '?enabled false})

  @?features)

;; Calling `seq` or otherwise iterating over a "plural" field returns a list of its child fields,
;; whose bindings can be read from the field using their names (quoted symbols). When providing
;; `:initial-children` for a plural field, each child should be a map of bindings as shown below.

(with-form [!form (?features :many {:name (str/upper-case ?name)})
            :init {?features [{'?name "Herman"}
                              {'?name "Sally"}]}]
  ;; below, we destructure each ?feature using :syms to access its bindings
  (for [{:as ?feature :syms [?name]} ?features]
    @?name))

;; Children are removed by passing a child instance to `forms/remove-many!`.

(with-form [!form (?items :many {:position ?position})]
  ;; add two items
  (forms/add-many! ?items '{?position 1} '{?position 2})
  ;; remove the first item
  (forms/remove-many! (first ?items))
  ;; Only the second item remains:
  @!form)

;; Example using a :many field:

(cljs
 (with-form [!form [[:db/add 1 :person/pets
                     ;; define a plural field by adding a :many key to the field.
                     ;; it should contain a "template" for each item in the list.
                     (?pets :many {:pet/id ?id
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

(with-form [!form {:animal-names (?names :many ?name
                                         :child-meta {:validators [(fn [value _]
                                                                     {:type :hint
                                                                      :content value})]})}]
  (forms/add-many! ?names '{?name "Toad"}
                   '{?name "Frog"})
  (->> (forms/messages ?names :deep true)
       (map :content)))

;; ## Server submission

;; `try-submit!` facilitates submission of a form's contents to a remote endpoint. What happens:
;; - We check if the form can be submitted  using `submittable?`
;; - If no, we 'touch' the form so that validation/error messages will appear
;; - If yes, we evaluate the promise and call `watch-promise`.

;; `watch-promise` sets `:loading?` to true and clears old remote messages immediately. When the
;; resolves, `:loading?` is removed and remote messages are set to (:message return-value)
;;
;; The following example includes buttons that show how to handle a successful or failed response.

(cljs
 (with-form [form {:name (?name :init "Sue")
                   :accepted-terms ?accepted}
             :form/validators [(fn [{:keys [accepted-terms]} _]
                                 (when-not accepted-terms
                                   {:type :invalid
                                    :content "Must accept terms"}))]]
   [:div
    [ui/input-text ?name]
    [:label.flex.flex-row.items-center [ui/input-checkbox ?accepted] "Accept terms?"]
    [:pre.text-xs.whitespace-pre-wrap (str @form)]
    (into [:div]
          (map ui/view-message (if (:loading? form)
                                 (forms/wrap-message "Loading...")
                                 (forms/visible-messages form))))
    [:button.bg-blue-500.text-white.p-3.m-3
     {:on-click #(forms/try-submit! form
                   (p/let [name @?name
                           result (p/timeout 500)]
                     {:message (str "Thanks, " name "!")}))}
     "Submit-Success"]
    [:button.bg-red-500.text-white.p-3.m-3
     {:on-click
      #(forms/try-submit! form
         (p/let [name @?name
                 result (p/timeout 500)]
           {:message
            {:type :error
             :content (str "Sorry " name ", an error occurred.")}}))}
     "Submit-Error"]]))

;; `forms/clear!` resets a form to its initial state

(cljs
 (with-form [!form {:a ?a
                    :b (?b :init "B")
                    :c ?c
                    :d (?d :many
                           [?e (?f :init "F") ?nil]
                           :init [{'?e "E"}])}
             :meta {:a {:init "A"}
                    ?c {:init "C"}
                    ?e {:init "E"}}]
   (str (= @!form
           (do (reset! ?a 1)
               (reset! ?b 2)
               (reset! ?c 3)
               (forms/add-many! ?d '{?e 4 ?f 5 ?nil 6})
               (forms/clear! !form)
               @!form)))))

;; ## Reagent

;; In ClojureScript, `with-form` uses [Reagent's](http://reagent-project.github.io)  `with-let` macro,
;; so that a form is created once (when a component mounts) and we can use the fields with input components.

;; PRs are welcome for integrations other than Reagent. All that is required is a macro which behaves
;; like `with-let` ("finally" behaviour is not required). See `inside-out.reagent` for macro implementation.

;; ## The 'form' macro

;; Our macros turn the "expression" of a form into a function whose arguments are its fields.
;; In fact, we can call a form as a function, and pass in a map of bindings:

(with-form [add (+ ?a ?b)]
  (add '{?a 1 ?b 2}))

;; This is probably most useful for REPL experiments or debugging. There is also a plain `form`
;; macro  which creates a form without bringing anything into scope.

(def add (forms/form (+ ?a ?b)))
(add '{?a 1 ?b 2})

;; A form's fields can be reached by looking them up as symbols:

('?a add)

;; ## Requirements and Constraints
;;
;; Or, what I was thinking about while writing this library.
;;
;; - A typical form is made up of multiple fields, which will be (somehow) combined for submission to some
;;   endpoint (could be REST, graphql, Om Next, Pathom...). There's no single "shape" of data that works for
;;   all use-cases. Originally I wanted to make a forms library that would work well for submitting triples
;;   to a DataScript/Datomic-like system, but didn't like how it felt when the system made assumptions about shape.
;; - A "field" is more than a value - it is information about the value & how it should be
;;   represented to a person ("metadata") which tends to be re-used in many places.
;; - Validation can happen on fields or forms. The messages returned by a validator function can depend on
;;   a field's value, and may be returned asynchronously. A server/endpoint can also return validation/error
;;   messages. We can re-use the machinery we build for validation for other purposes, like value-dependent
;;   hints/instructions.
;; - It's necessary to track "touched" and "focused" states in order to show/hide messages appropriately.
;; - Most of the time we can infer what "attribute" a field would correspond to by its location in a parent
;;   structure (eg. if the field is in the value position of a map, or in a `:db/add` vector)
;;
;;
;; # 🙏 NextJournal
;;
;; This library was made in collaboration with the folks at [NextJournal](https://www.nextjournal.com),
;; who also make [Clerk](https://github.com/nextjournal/clerk), the happy little tool driving this notebook.
;;
;; Thanks to NextJournal for supporting this work and allowing it to be open-sourced.



