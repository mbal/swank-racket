;; WORKAROUND for getpid being available only in unixes.
(module getpid mzscheme
  (require (lib "foreign.ss")) (unsafe!)
  (provide getpid)
  (define getpid
    (if (eq? (system-type) 'windows)
      (get-ffi-obj "GetCurrentProcessId" (ffi-lib "kernel32") (_fun -> _int))
      (get-ffi-obj "getpid" #f (_fun -> _int)))))
