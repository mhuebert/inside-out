# Inside-Out: A Clojure forms library

(alpha)

See [Inside-Out: Documentation](https://github.com/mhuebert/inside-out/blob/main/dev/inside_out/notebook.cljc)

## Usage 

clj deps:

```clj 
mhuebert/inside-out {:git/sha "..."}
```

## Minimal example: 

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