;;; This loader constructs a new user-initial-environment.

(set! user-initial-environment
      (extend-top-level-environment system-global-environment))
(ge user-initial-environment)

(load "eq-properties")
(load "advice")

(load "1-1")
(load "1-2")
