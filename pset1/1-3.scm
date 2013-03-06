; 1.3.a
(define (authorization-wrapper-improved procname proc args)
  (cond ((member (eq-get proc 'authorization-key)
    (or (eq-get current-user-id 'authorizations)
        '()))
    (apply proc args))
  (else
    (error "Unauthorized access" current-user-id procname))))

(define (add-auth-to-func func key)
  (eq-put! func 'authorization-key (md5-string key)))

(define (add-auth-keys-to-user user keys)
  (eq-put! user 'authorizations (map (lambda (key) (md5-string key)) keys)))

(define current-user-id 3)

(display "\nbefore\n")
(display (sin 1))

(add-auth-to-func sin "ok-to-sin")
(advise-n-ary sin authorization-wrapper-improved)
(display "\nafter adding auth to sin\n")
(display (sin 1))

(add-auth-keys-to-user current-user-id (list "ok-to-sin" "ok-to-cos" "ok-to-atan" "ok-to-read-files"))
(display "\nafter adding auth key to current-user-id\n")

(display (sin 1))


; 1.3.b
; We would want to require authorization for directly accessing various resources, such as the filesystem, any connected databases, or the network. This way, if one program on the server is compromised, it can do minimal damage to other programs on the machine.
; Specifically, we should require authorization for the eval function. This allows execution of arbitrary code, and could be very dangerous if an adversary can gain access to it.