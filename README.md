# qubits

Spooky action at a distance, in your repl!

## Hey okay let me have some of these qubits

Alright let's see.

``` clojure
(require '[com.gfredericks.qubits.objects :refer :all])

(def foo (qubit "foo"))

;; here is the qubit we just made
foo
=> #<Qubit-foo: 0>

;; we can ask for its value
(observe foo)
=> 0

;; and again if we like
(observe foo)
=> 0
```

### Can I do anything interesting with this qubit?

There are a few "gates" that we can send the qubit through to
potentially change its state.

``` clojure
;; The "X" gate changes the 0 value to 1 and vice versa. It's a "NOT"
(X foo)
=> #<Qubit-foo: 1>

(X foo)
=> #<Qubit-foo: 0>

;; The "Z" gate flip's the qubit's phase. What the hell does that mean?
;; It doesn't seem to have any effect.
(Z foo)
=> #<Qubit-foo: 0>

(doto foo Z Z Z Z Z)
=> #<Qubit-foo: 0>

;; Well nevermind that for now.

;; The H gate is trickier. If the qubit has a value of 0, it puts it into
;; equal superposition of 0 and 1.
(H foo)
=> #<Qubit-foo: ?>

;; We can observe it to force it to decide its value:
(observe foo)
=> 1

foo
=> #<Qubit-foo: 1>

;; Repeated observations give the same value
(repeatedly 20 #(observe foo))
=> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)

;; But repeated applications of the H gate followed by an observation
;; are nondeterministic
(repeatedly 20 #(-> foo H observe))
=> (1 1 1 0 0 0 0 0 1 1 0 1 1 0 1 1 1 1 0 0)

;; But but but! What if we do two H gates in a row before observing?
(repeatedly 20 #(-> foo H H observe))
=> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)

;; How about three of them?
(repeatedly 20 #(-> foo H H H observe))
=> (1 1 0 1 1 0 1 1 1 0 1 1 0 0 1 1 0 0 0 0)

;; Or what if we observe in between two H's?
(repeatedly 20 #(-> foo H (doto observe) H observe))
=> (0 1 0 0 0 1 1 0 0 0 1 0 0 0 1 1 0 1 0 0)
```

### WAT

Look I don't have time to make sense of all that. Let's try entangling
some qubits!

``` clojure
;; We'll start off with a fresh pair of qubits.
(def foo (qubit "foo"))
(def bar (qubit "bar"))

[foo bar]
=> [#<Qubit-foo: 0> #<Qubit-bar: 0>]

;; So they're both starting off 0
(map observe [foo bar])
=> (0 0)

;; Now these gates we were using before can take extra arguments,
;; which are controls -- the gate is only effective when all the
;; controls have a value of 1.

;; This X has no effect since bar is 0
(X foo bar)
=> #<Qubit-foo: 0>

[foo bar]
=> [#<Qubit-foo: 0> #<Qubit-bar: 0>]

;; But if we change bar to 1 first:
(X bar)
=> #<Qubit-bar: 1>

;; And then try it:
(X foo bar)
=> #<Qubit-foo: 1>

[foo bar]
=> [#<Qubit-foo: 1> #<Qubit-bar: 1>]

;; Okay so then let's set them back to 0:
(X foo)
#<Qubit-foo: 0>
(X bar)
#<Qubit-bar: 0>

;; Now we put the foo qubit in a superposition.
(H foo)
#<Qubit-foo: ?>

;; Just to check what's going on, we can ask for its observation probabilities
(probabilities foo)
=> {0 0.4999999999999999, 1 0.4999999999999999} ; 50/50, modulo some floating point nonsense

;; whereas bar is still hanging out at 0:
(probabilities bar)
=> {0 1.0, 1 0}

;; Now let's flip bar's value with foo (who is in superposition) as the control:
(X bar foo)
=> #<Qubit-bar: ?>

[foo bar]
=> [#<Qubit-foo: ?> #<Qubit-bar: ?>]

(probabilities foo)
=> {0 0.4999999999999999, 1 0.4999999999999999}

(probabilities bar)
=> {0 0.4999999999999999, 1 0.4999999999999999}

;; So they're both in a superposition. But because bar's value depends on foo,
;; they're effectively entangled. Observing one will necessarily effect the
;; possible observations of the other.

;; observing foo tells us what its value was when the X gate was applied to bar --
;; i.e., we learn whether or not the X gate was actually applied
(observe foo)
=> 1

;; Since we observed a 1, we know the X gate _was_ applied, so bar should be 1
;; as well
(observe bar)
=> 1

[foo bar]
=> [#<Qubit-foo: 1> #<Qubit-bar: 1>]

;; The qubits are no longer entangled.

;; To confirm that we didn't get the same outcome from both
;; observations by chance, we can do it a few times:

(repeatedly 10 (fn []
                 ;; using fresh qubits here to make sure they start off 0
                 (qubits [q1 q2]
                   (H q1)
                   (X q2 q1)
                   (map observe [q1 q2]))))
=> ((0 0) (0 0) (1 1) (0 0) (0 0) (1 1) (0 0) (1 1) (0 0) (1 1))
```

### ...

I dunno man look play around with it yourself and see if it makes any
more sense.

## License

Copyright © 2013 Gary Fredericks

Distributed under the Eclipse Public License, the same as Clojure.
