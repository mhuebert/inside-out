# io: Human-AI Collaborative Workflows

The `io` module extends inside-out with a DSL for coordinating human-AI workflows with transient UIs.

## Quick Start

```clojure
(require '[inside-out.io :as io]
         '[inside-out.io.executor :as exec]
         '[inside-out.io.ui :as ui])

;; Define a workflow
(def !wf (io/make-reactive-workflow
           :my-workflow
           '{:topic (?topic :ask "What would you like to explore?")
             :outline ($outline :ai "Create outline for {?topic}" :approve)
             :draft ($draft :ai "Write draft following {$outline}")}))

;; Create an executor
(def executor (exec/make-executor {:!workflow !wf}))

;; Run until human input needed
(exec/run! executor)

;; Provide human input
(exec/submit-input! executor '?topic "The Future of AI")

;; Approve AI output
(exec/submit-approval! executor '$outline true)
```

## Core Concepts

### Slots

Slots are named positions in a workflow that get filled with values:

- **`?name`** - Human input slot (user provides value)
- **`$name`** - AI generation slot (AI generates value)
- **`?`** - Anonymous human slot (each occurrence is distinct)
- **`$`** - Anonymous AI slot (each occurrence is distinct)

### Metadata

Slots can have metadata that controls their behavior:

```clojure
(?topic :ask "What's your topic?")     ; Prompt for human
($draft :ai "Write about {?topic}")    ; Instructions for AI
($plan :ai "Create plan" :approve)     ; Requires human approval
(?choice :ui :select :options [...])   ; UI hint
```

### Dependencies

Dependencies are **implied by reference**:

```clojure
{:topic ?topic
 :outline ($outline :ai "Outline for {?topic}")   ; depends on ?topic
 :draft ($draft :ai "Draft from {$outline}")}     ; depends on $outline
```

The executor automatically determines order from the dependency graph.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        io Form                              │
│  {:topic ?topic                                             │
│   :outline ($outline :ai "..." :approve)                    │
│   :draft ($draft :ai "...")}                                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    io/build-dependency-graph                 │
│  Analyzes form → extracts slots → finds dependencies        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      WorkflowState                          │
│  - slots: {sym → SlotState}                                 │
│  - order: [topologically sorted syms]                       │
│  - values: {sym → completed value}                          │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                        Executor                             │
│  - Identifies ready slots                                   │
│  - Invokes AI client for $slots                             │
│  - Triggers UI callbacks for ?slots                         │
│  - Handles approval gates                                   │
└─────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┴───────────────┐
              ▼                               ▼
┌─────────────────────────┐     ┌─────────────────────────┐
│     Transient UIs       │     │      Persistence        │
│  - text-input           │     │  - LocalStorage         │
│  - textarea             │     │  - serialize/deserialize│
│  - select               │     │  - auto-save            │
│  - approval-panel       │     │  - export/import        │
└─────────────────────────┘     └─────────────────────────┘
```

## Modules

### `inside-out.io`

Core namespace with:
- Slot type detection (`human-slot?`, `ai-slot?`)
- Form analysis (`extract-slots`, `build-dependency-graph`)
- Workflow state (`WorkflowState`, `SlotState`)
- State transitions (`set-slot-value`, `approve-slot`, etc.)
- Serialization (`serialize-workflow`, `deserialize-workflow`)

### `inside-out.io.executor`

Runtime orchestration:
- `make-executor` - Create executor with callbacks
- `run!` - Run until human input needed
- `submit-input!` - Provide human input
- `submit-approval!` - Approve/reject AI output
- `IAIClient` protocol for AI integration

### `inside-out.io.ui`

Reagent components (Tailwind/DaisyUI):
- `progress-bar`, `progress-steps` - Progress visualization
- `slot-list`, `slot-card` - Slot state display
- `dependency-graph` - Visual dependency graph
- `text-input`, `textarea-input`, `select-input` - Human inputs
- `approval-panel` - AI output review
- `workflow-view` - Complete workflow UI

### `inside-out.io.persist`

Persistence layer:
- `IWorkflowStorage` protocol
- LocalStorage implementation
- `WorkflowManager` for managing multiple workflows
- Auto-save, export/import
- Pre-defined templates

## Running the Demo

```bash
# Start shadow-cljs
npx shadow-cljs watch io-demo

# Open browser
open http://localhost:8766/io_demo.html
```

## Workflow Templates

Built-in templates in `persist/templates`:
- `:research` - Research assistant
- `:writing` - Writing with revision
- `:brainstorm` - Ideation and evaluation
- `:planning` - Project planning

## Extending

### Custom AI Client

```clojure
(defrecord MyAIClient [api-key]
  exec/IAIClient
  (generate [this prompt opts]
    ;; Return promise resolving to generated text
    (call-my-api api-key prompt opts)))
```

### Custom Storage Backend

```clojure
(defrecord MyStorageBackend []
  persist/IWorkflowStorage
  (save-workflow [_ id data] ...)
  (load-workflow [_ id] ...)
  (delete-workflow [_ id] ...)
  (list-workflows [_] ...))
```

### Custom Transient UIs

Add new UI types by extending the `transient-ui` component to handle additional `:ui` metadata values.
