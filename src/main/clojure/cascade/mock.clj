; Copyright 2009 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns #^{:doc "Wrapper for EasyMock"}
  cascade.mock
  (:import (org.easymock EasyMock IMocksControl))
  (:use
    (cascade collection-utils fail)
    (cascade.internal utils)))

(defn new-mock
  [#^IMocksControl control #^String mock-name #^Class mock-class]
  (.createMock control mock-name mock-class))

(defn mock-init
  [control mock-name mock-class]
  `(~mock-name (new-mock ~control ~(name mock-name) ~mock-class)))

(defmacro with-mocks
  "Defines a set of mocks and executes training and test code for them. mock-defs is a vector of alternating
  symbols and classes to mock, an implicit let will be used to assign a mock instance to each symbol.
  The forms identify the code to execute, each form is a list that starts with a keyword that identifies
  the execution state; the remaining forms are executed at that stage. The two stages are :train and :test.
  The macro ensures that the controls are switched to replay mode after training, and are verified
  after testing. In addition, a :binding clause will identify var symbols and local bindings in effect
  for the :train and :test phases (the symbols and bindings should not be in a vector)."
  [mock-defs & forms]
  (fail-unless (even? (count mock-defs)) "Must have an even number of mock defs (alternating symbols and classes).")
  (let [control (gensym "control")
        mock-pairs (partition 2 mock-defs)
        mock-inits (mapcat (fn [[mock-name mock-class]] (mock-init control mock-name mock-class)) mock-pairs)
        mapped-forms (list-to-map-of-seqs [:binding :train :test] forms)]
        `(let [~control (EasyMock/createControl)
              ~@mock-inits]
            (binding [~@(mapped-forms :binding)]
              ~@(mapped-forms :train)
              (.replay ~control)
              ~@(mapped-forms :test))
            (.verify ~control))))

(defmacro expect
  "Used during Mock object training; evaluates all but the last form (which is expected to be an
invocation on a Mock object); the last form is the return value from the evaluation."
  [& forms]
  (let [eval-forms (butlast forms)
        result (last forms)]
    `(.andReturn (EasyMock/expect ~eval-forms) ~result)))