^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns inside-out.notebook
  {:nextjournal.clerk/toc true
   :nextjournal.clerk/no-cache true}
  (:require [clojure.string :as str]
            [inside-out.forms :as forms]
            [inside-out.ui :as ui :refer [with-form cljs]]
            [nextjournal.clerk :as-alias clerk]
            [promesa.core :as p]))

;; # Inside-Out: a Clojure forms library
;;
;; _alpha - [feedback welcome](https://github.com/mhuebert/inside-out/discussions)_
;;
;; **Inside-Out** is a new kind of tool for making interactive forms. We start by declaring the "shape" of data
;; we want, including `?variables` which can be set interactively.

;; For example, let's say we want a simple map containing a person's name:

;; ```clj
;; {:person/name ?name}
;; ```

;; Let's use the `with-form` macro to bind this data structure, as well as its variables, in one step:

(cljs
 (with-form [?person {:person/name ?name}]
   (reset! ?name "Peter")
   @?person))

;; What do we see above?
;; 1. `with-form` creates a new "form" called `?person`, which returns `{:person/name ?name}` when dereferenced.
;; 3. The form has one field, `?name`, which behaves like an atom. Any symbol that starts with `?` will become a field.

;; In the browser, we can then use the field in an input component:

(cljs
 (with-form [?person {:person/name ?name}]
   [:div.p-1
    [:input.border.p-3
     {:value @?name
      :on-change #(reset! ?name (.. % -target -value))
      :placeholder "Name"}]
    [:pre (str @?person)]]))

;; A form can take any shape, and fields can appear more than once. Here we use `?pet-id` twice, to build up a
;; datomic transaction:

(with-form [!tx [[:db/add 1 :person/pet ?pet-id]
                 [:db/add ?pet-id :pet/name ?pet-name]]]
  (reset! ?pet-id 2)
  (reset! ?pet-name "Fido")
  @!tx)

;; Unlike common approaches that rely on concrete "paths" or "cursors" into a static data structure,
;; inside-out forms are arbitrary _expressions_ that are computed to produce the form's output.

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

;; ## Installation & Usage
;;
;; ```clj
;; ;; deps
;; {io.github.mhuebert/inside-out {:git/sha "$GIT_SHA"}}
;;
;; ;; namespace
;; (ns my-app
;;   (:require [inside-out.forms.reagent :refer [with-form]]
;;             [inside-out.forms :as forms]))
;;```
;;

;; ## Metadata
;;
;; Inside-out supports metadata to handle common concerns like validation.

;; There are three ways to specify metadata:

;; (1) Inline, by wrapping the field in a list: `(?name :init "Peter")`.
;; Initial values can be supplied via `:init` metadata.

(with-form [contact-info {:name (?name :init "Peter")}]
  @contact-info)

;; (2) Add metadata keys after the form:

(with-form [foo (str ?first-name " " ?last-name)
            :init {?first-name "Peter"
                   ?last-name "Rabbit"}]
  @foo)

;; (3) Add a `:meta` map after the form:

(with-form [?person {:name ?name}
            :meta {?name {:init "Cottontail"}}]
  @?person)

;; We can read metadata by looking up a key on a field, as seen above in `(:required ?email)`.

(with-form [form {:name (?name :required true)}]
  (:required ?name))

;; ### Attribute metadata
;;
;; We can define metadata for _attributes_ like `:person/name`, which are then _inferred_
;; from the shape of a data structure:
;; 1. when a field is in the value position in a map, eg. `{<attribute> ?field}`,
;; 2. when a field is in the value position of a `:db/add` vector:
;;    `[:db/add <id> <attribute> ?field]`

(with-form [form {:pet/id ?pet-id}]
  ;; attribute is inferred from key in a map
  (:attribute ?pet-id))

(with-form [form [[:db/add 1 :person/name ?name]]]
  ;; attribute is inferred based on position in a :db/add vector
  (:attribute ?name))

;; Defining a map of attribute-metadata:

(def app-attributes {:person/name {:label "Your name"}
                     :person/phone {:validators [(fn [value _]
                                                   ;; ensure value is a phone number
                                                   )]}})

;; We can pass that in as a :meta option:

(with-form [form [[:db/add 1 :person/name ?name]]
            :meta app-attributes]
  (:label ?name))

;; To supply initial values for keys in a map (via attribute inference):

(with-form [?foo {:a ?a :b ?b}
            :init {:a 1 :b 2}]
  @?foo)

;; In practice, you'll probably want to define attribute metadata once in your app and re-use it
;; everywhere. To support this use-case, fields inherit from a globally defined var
;; `inside-out.forms/global-meta`.

;; in ClojureScript, we would `set!` the var during app initialization:
(cljs

 ;; inside your app's initialization code - can be a map or a function.
 (forms/set-global-meta! {:person/name {:field/label "Your name"}})

 [:pre (str inside-out.forms/global-meta)])

;; Now it is available globally:

(cljs
 (with-form [?tx [[:db/add 1 :person/name ?name]]]
   (:field/label ?name)))

;; ## Validation

;; Fields may specify a list of `:validators` are called on-demand when we read
;; a field's "messages" via `(forms/messages ?field)`.

(with-form [?tx [[:db/add 1 :person/name ?name]]
            :validators {?name [(forms/min-length 3)]}]

  (reset! ?name "ma")
  (->> (forms/messages ?name)
       (map :content)))

;; Each validator is a function which is passed the field's current value and a map of
;; the other fields in the form (which must be dereferenced to read their values):

(defn validate-child-age [value {:syms [?parent-age]}]
  (when (>= value @?parent-age)
    {:type :invalid
     :content "Child must be younger than parent"}))

(with-form [?tx [{:db/add 1
                  :parent/age ?parent-age}
                 {:db/id 2
                  :child/parent 1
                  :child/age (?child-age :validators [validate-child-age])}]]
  (reset! ?parent-age 10)
  (reset! ?child-age 20)
  (->> (forms/messages ?child-age)
       (map :content)))


;; Validators for the form itself can be passed using a :form/validators option

(with-form [?contact {:phone/mobile ?mobile
                      :phone/landline ?landline}
            :form/validators [(fn [_ {:syms [?mobile ?landline]}]
                                (when-not (or @?mobile @?landline)
                                  {:type :invalid
                                   :content "At least one phone number must be supplied"}))]]
  ;; form becomes valid after adding a value for ?mobile
  [(forms/valid? ?contact)
   (do (reset! ?mobile "+49 555 5555555")
       (forms/valid? ?contact))])

;; A `:required` option may be passed with a list of required fields

(with-form [form {:name ?name :email ?email :phone ?phone}
            :required [?name ?email ?phone]]
  (forms/messages form :deep true))

;; Or, include `:required true` in a field's metadata

(with-form [form {:name (?name :required true)
                  :email ?email}
            :meta {?email {:required true}}]
  (forms/messages form :deep true))

;; ### Validator functions

;; A validator is a function with the signature `(value, context) => [...message]` where `context`
;; is a map of sibling fields.
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
  :when #{:always :focused :touched}}                       ;; default is :focused
 {:type :invalid
  :content "A value that should be rejected"}]

;; ### Options

;; Options may be added to a validator function by wrapping with `forms/validator`.
;;
;; ```clj
;; (forms/validator f :debounce-ms 300)
;; ```
;;
;;- `:compute-when [...condition]` - only compute when at least one condition
;;  is met (`:focused`, `:blurred`, `:touched`)
;;- `:debounce-ms <ms>` (only relevant when `:async true`) waits until function
;;  hasn't been called for the given period of time before evaluating again.
;; - `:on-blur` - computes when a field is blurred
;;

(cljs
 (with-form [form {:name ?name}
             :validators {?name [(-> (fn [value _] (forms/message :info (str "Hello, " value)))
                                     (forms/validator :compute-when [:focused]))]}]
   [:div.p-1 [ui/input-text ?name]]))

;; Async example

(cljs
 (def check-domain
   (-> (fn [value _]
         (p/do (p/delay 200)
               (if (even? (count value))
                 (forms/message :info "Even: valid" :visibility :always)
                 (forms/message :invalid "Odd: invalid" :visibility :always))))
       (forms/validator :async true
                        :debounce-ms 500
                        :compute-when [:touched]))))

;; forms/try-submit+ waits for any async validation to finish before continuing

(cljs
 (with-form [form {:domain (?domain :validators [check-domain])}]
   [:form.p-1 {:on-submit (fn [^js e]
                            (.preventDefault e)
                            (forms/try-submit+ form (prn :submitting @form)))}
    [ui/input-text ?domain]
    [:pre (str (forms/messages ?domain))]
    [:pre "submittable? " (str (forms/submittable? form))]]))


;; ## Components
;;
;; Helper functions are provided for managing a component's blur and focus events to track
;; the "touched" status of a field, useful for limiting visibility of validation messages.

;; A field is considered "touched" if a user has already interacted with it, or if its parent
;; form is touched (we "touch" a form itself when a user tries to submit it).

(cljs
 (defn managed-text-input
   "A text-input element that reads metadata from a ?field to display appropriately"
   [?field attrs]
   (let [messages (forms/visible-messages ?field)]
     [:<>
      [ui/input-text-element
       (merge {:placeholder (or (:label ?field)
                                (-> (:sym ?field)
                                    str
                                    (subs 1)
                                    str/capitalize))
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

   [:form.p-1
    [managed-text-input ?name]
    [:pre (str "valid? " (forms/valid? form))]
    [:pre (str @form)]]))

;; ## Plural fields (subforms)

;; A field may contain multiple "child" elements. Define a plural field by passing a
;; `:many` option with the template for each child. If present, `:init` should be
;; a collection of bindings - maps of the shape `{?field-name <value>}`.

(with-form [?form (?features :many {:name ?feature-name}
                             :init [{'?feature-name "My Great Feature"}])]
  @?features)

;; Add a child using `forms/add-many!`:

(with-form [?form (?features :many {:name ?feature-name})]

  ;; add a child:
  (forms/add-many! ?features {'?feature-name "Powder Coated"})

  @?features)

;; Calling `seq` on a plural field returns a list of its children, each of which is a form.
;; Typically we would destructure each child using `:syms` to bring its fields into scope.

(with-form [?form (?features :many {:name (str/upper-case ?name)}
                             :init [{'?name "Herman"}
                                    {'?name "Sally"}])]
  (for [{:as ?feature :syms [?name]} ?features]
    @?name))

;; Remove a child by passing it to `forms/remove-many!`.

(with-form [?form (?features :many {:name (str/upper-case ?name)}
                             :init [{'?name "Herman"}
                                    {'?name "Sally"}])]
  (forms/remove-many! (first ?features))
  @?form)

;; Interactive example:

(cljs
 (with-form [?form [[:db/add 1 :person/pets
                     ;; define a plural field by adding a :many key to the field.
                     ;; it should contain a "template" for each item in the list.
                     (?pets :many {:pet/id ?id
                                   :pet/name (?name :init "Fido")})]]]

   [:div.p-1
    [ui/show-code (str @?form)]

    (doall
     ;; call (seq ?pets) to get a list of fields, which can be destructured using :syms
     ;; to get the child bindings.
     (for [{:as ?pet :syms [?id ?name]} ?pets]
       [:div.flex.items-center.my-2
        {:key @?id}
        [managed-text-input ?name {:placeholder "Name"}]
        [:div.text-red-500.hover:underline.hover:cursor-pointer.mx-3.font-bold
         ;; call forms/remove-many! to remove an item
         {:on-click #(forms/remove-many! ?pet)} "X"]]))

    [:div.my-3.text-blue-500.hover:underline.hover:cursor-pointer
     ;; to add an item, call form/add-many! with ?pets and a map of bindings,
     ;; using quoted symbols for keys {'?field, value}
     {:on-click #(forms/add-many! ?pets {'?id (rand-int 1000)})} "Add Pet"]]))

;; To specify metadata targeting the children of a plural field, use the :child-meta key:

(with-form [?form {:animal-names (?names :many ?name
                                         :child-meta {:validators [(fn [value _]
                                                                     {:type :hint
                                                                      :content value})]})}]
  (forms/add-many! ?names '{?name "Toad"}
                   '{?name "Frog"})
  (->> (forms/messages ?names :deep true)
       (map :content)))

;; ## Server submission

;; `watch-promise` communicates the status of a promise through a field's metadata as follows:
;; 1. `:loading?` is immediately set to `true` and `:remote-messages` are cleared,
;; 2. When the promise resolves, `:loading?` is removed and `:remote-messages` are set to `(:message return-value)`.

;; The `try-submit+` macro builds on this to manage submission of a form to a remote endpoint:
;; 1. We check if the form can be submitted using `submittable?`,
;; 2. If not, we `touch!` the form so that validation/error messages will appear,
;; 3. If yes, we evaluate the promise and wrap it with `watch-promise`.

;; The following example includes buttons that show how to handle a successful or failed response.

(cljs
 (with-form [?form {:name (?name :init "Sue")
                    :accepted-terms ?accepted}
             :form/validators [(fn [{:keys [accepted-terms]} _]
                                 (when-not accepted-terms
                                   "Must accept terms"))]]
   [:div.p-1
    [ui/input-text ?name]
    [:label.flex.flex-row.items-center [ui/input-checkbox ?accepted] "Accept terms?"]
    [:pre.text-xs.whitespace-pre-wrap (str @?form)]
    (into [:div]
          (map ui/view-message (if (:loading? ?form)
                                 (forms/wrap-messages "Loading...")
                                 (forms/visible-messages ?form))))
    [:button.bg-blue-500.text-white.p-3.m-3
     {:on-click #(forms/try-submit+ ?form
                   (p/do (p/delay 500)
                         (forms/message :info (str "Thanks, " @?name "!"))))}
     "Submit-Success"]
    [:button.bg-red-500.text-white.p-3.m-3
     {:on-click
      #(forms/try-submit+ ?form
         (p/do
           (p/delay 500)
           (forms/message :error (str "Sorry " @?name ", an error occurred."))))}
     "Submit-Error"]]))

;; `forms/clear!` resets a form to its initial state

(cljs
 (with-form [form {:a ?a
                   :b (?b :init "B")
                   :c ?c
                   :d (?d :many
                          [?e (?f :init "F") ?nil]
                          :init [{'?e "E"}])}
             :meta {:a {:init "A"}
                    ?c {:init "C"}
                    ?e {:init "E"}}]
   (str (= @form
           (do (reset! ?a 1)
               (reset! ?b 2)
               (reset! ?c 3)
               (forms/add-many! ?d '{?e 4 ?f 5 ?nil 6})
               (forms/clear! form)
               @form)))))

;; ## Reagent

;; In ClojureScript, `with-form` uses [Reagent's](http://reagent-project.github.io)  `with-let` macro,
;; so that a form is created once (when a component mounts) and we can use the fields with input components.
;; We also use reagent's reactive atoms and reactions for reactivity.

;; If you'd like to implement inside-out for view libraries other than Reagent, please get in touch.

;; ## The 'form' macro

;; Our macros turn the "expression" of a form into a function whose arguments are its fields.
;; In fact, we can call a form as a function, and pass in a map of bindings:

(with-form [add (+ ?a ?b)]
  (add '{?a 1 ?b 2}))

;; This is probably most useful for REPL experiments or debugging. There is also a plain `form`
;; macro  which creates a form without bringing anything into scope.

(def add (forms/form (+ ?a ?b) :init {?a 1 ?b 1}))

(add '{?a 1 ?b 2})

;; A form's fields can be reached by looking them up as symbols:

@('?a add)

;;
;; # 🙏 NextJournal
;;
;; This library was made in collaboration with the folks at [NextJournal](https://www.nextjournal.com),
;; who also make [Clerk](https://github.com/nextjournal/clerk), the happy little tool driving this notebook.
;;
;; Thanks to NextJournal for supporting this work and allowing it to be open-sourced.



;; # More examples

;; Conditionally validating fields

(cljs
 (with-form [?field (merge {:type (?type :init :text)}
                           (case ?type
                             :text {:content ?text}
                             :image-url {:image-url ?image-url}))
             :required [?type]
             :validators {?type #{:text :image-url}
                          ?text (fn [v {:syms [?type]}]
                                  (when (and (= @?type :text)
                                             (not (string? v)))
                                    "Must be a string"))
                          ?image-url (fn [v {:syms [?type]}]
                                       (when (and (= @?type :image-url)
                                                  (not (some-> v (str/starts-with? "https://"))))
                                         "Must be a secure URL beginning with https://"))}]

   [:div.flex-v.gap-2.w-64.p-1
    (str "type: " @?type)
    [:button.p-1.bg-blue-700.text-white.rounded.mb-1
     {:on-click #(swap! ?type {:text :image-url
                               :image-url :text})}
     "Toggle type!"]
    (case @?type
      :text [:input {:placeholder "Text"
                     :on-change (fn [^js e] (reset! ?text (.. e -target -value)))
                     :value @?text}]
      :image-url [:input {:placeholder "Image URl"
                          :on-change (fn [^js e] (reset! ?image-url (.. e -target -value)))
                          :value @?image-url}])

    ;; show all messages for the form
    (->> (forms/messages ?field :deep true)
         (map ui/view-message)
         (into [:<>]))]))