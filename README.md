# Inside-Out: A Clojure forms library

See [Inside-Out: Documentation](https://github.com/mhuebert/inside-out/blob/main/dev/inside_out/notebook.cljc)

```clj 
io.github.mhuebert/inside-out {:git/sha "SHA"}
```

A tool for writing web forms. Syntax is minimal, and lets us focus on essential concerns: what is our target
data structure? From what fields is it composed? How should these fields behave?

### Features

1. Concise syntax for defining a form and its fields in one step. Each field stores a value, and behaves
   like an atom (read/write with `@deref` and `reset!`). The form's "output" is computed from its fields
   and can take any shape.

2. A metadata system that encourages data-driven design & code re-use, while handling common concerns like
   validation, hints, and error messages to support a high quality UX. Forms should be "driven by data"
   but avoid impenetrable indirection - don't break "jump to source".

### Minimal example: 

```clj 
(ns my-app.core 
  (:require [inside-out.reagent :refer [with-form]]))
  
(with-form [contact-info {:person/name ?name}]
  [:form {:on-submit (fn [e]
                       (.preventDefault e)
                       (js/alert (str "submitted: " @contact-info)))}
   [:input
    {:value @?name
     :on-change (fn [e] (reset! ?name (.. e -target -value)))
     :placeholder "Name:"}]])
```

## Dev

1. with [babashka](https://github.com/babashka/babashka) installed, run `bb dev`. 
2. open a REPl connection to the shadow process (port is in `.shadow-cljs/nrepl.port`)
3. eval `(start)` in `dev/dev.clj` to build the notebook  in `dev/inside_out/notebook.clj` and live-reload changes.
