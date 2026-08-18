;; SPDX-FileCopyrightText: 2026 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define-syntax when/null
  (syntax-rules ()
    ((_ cond body ...)
     (if cond
         (begin body ...)
         (list)))))

(define-syntax false-if-error
  (syntax-rules ()
    ((_ body ...)
     (call/cc
      (lambda (k)
        (with-exception-handler
            (lambda (e)
              (k #f))
          (lambda _
            body ...)))))))

(define (assoc-ref key list)
  (if (assoc key list)
      (cadr (assoc key list))
      #f))

(define ? display)

(define (take-5 lst)
  (take lst (min (length+ lst) 5)))

(define (take-10 lst)
  (take lst (min (length+ lst) 10)))

(define (print-abridged object)
  (if (> (length object) 5)
      (begin (? "(")
             (map (lambda (e)
                    (? e) (? " "))
                  (take-5 object))
             (when (> (length object) 5)
               (? "[...and ") (? (- (length object) 5)) (? " more]"))
             (? ")"))
      (? object)))

(define-checked (to-string-with object (proc procedure?)) => (string?)
  (call-with-port (open-output-string)
    (lambda (p)
      (proc object p)
      (get-output-string p))))

;;; inspect-properties

(define (object-properties object)
  ;; hash-by-identity should be a good enough approximation because
  ;; the current implementation of hash-by-identity takes object’s
  ;; address modulo fixnum size. It’s fine.
  `((id ,(hash-by-identity object))
    (location ,(hash-by-identity object))
    (size ,(object-size object))
    (type ,(type-name (type-of object)))
    (write ,(to-string-with object write))
    (display ,(to-string-with object display))))

(define-checked (number-properties (object number?))
  `((real-part ,(real-part object))
    (imag-part ,(imag-part object))
    ,@(when/null (rational? object)
                 `((numerator ,(numerator object))
                   (denominator ,(denominator object))
                   (real-sign ,(check-case
                                object
                                (negative? -1)
                                (zero? 0)
                                (positive? 1)))))
    (real-base 2)
    ;; Chibi encodes flonums as double-s
    (real-precision 53)
    ,@(when/null (integer? object)
                 `((integer-length ,(integer-length object))))
    ,@(when/null (and (integer? object)
                      (<= object #x10FFFF))
                 `((integer->char ,(integer->char object))))
    ;; integer-object seems to be generally impossible:
    ;; https://github.com/ashinn/chibi-scheme/issues/1136
    (display-2 ,(number->string object 2))
    (display-8 ,(number->string object 8))
    (display-16 ,(number->string object 16))))

(define-checked (boolean-properties (object boolean?))
  ;; sexp.h has these, but I daren’t go there:
  ;; #define SEXP_FALSE  SEXP_MAKE_IMMEDIATE(0) /* 14 0x0e */
  ;; #define SEXP_TRUE   SEXP_MAKE_IMMEDIATE(1) /* 30 0x1e */
  `((boolean->integer ,(if object 1 0))))

(define-checked (pair-properties (object pair?))
  (let* ((truly-last (if (circular-list? object)
                         '()
                         (let loop ((object object))
                           (if (pair? object)
                               (loop (cdr object))
                               object)))))
    `((car ,(car object))
      (cdr ,(cdr object))
      ,@(when/null (and (proper-list? object)
                        (not (circular-list? object)))
                   `((last ,(last object))
                     (last-pair ,(last-pair object))
                     (length ,(length object))))
      ,@(when/null (not (circular-list? object))
                   `((length+ ,(length+ object))
                     ;; Elements of the list, except the dotted tail
                     ,@(let loop ((length (length+ object))
                                  (idx 0)
                                  (object object))
                         (if (zero? length)
                             '()
                             (cons (list idx (car object))
                                   (loop (- length 1)
                                         (+ idx 1)
                                         (cdr object)))))))
      ,@(when/null (dotted-list? object)
                   `((dotted-last ,truly-last))))))

(define-checked (symbol-properties (object symbol?))
  `((symbol->string ,(symbol->string object))
    ;; That’s crude and likely quite broken, but whatever
    ,@(call/cc
       (lambda (k)
         (with-exception-handler
             (lambda (e)
               (k '()))
           (lambda _
             `((symbol-value ,(eval object (interaction-environment))))))))))

(define-checked (char-category (c char?))
  (cond
   ((and (char-alphabetic? c)
         (char-upper-case? c))
    'Lu)
   ((and (char-alphabetic? c)
         (char-lower-case? c))
    'Lu)
   ;; TODO title case
   ((char-numeric? c)
    ;; Kaktoviks are No, but there’s no Chibi way to detect that?
    'Nd)
   ((and (char-whitespace? c)
         (> 31 (char->integer c)))
    'Zs)
   (else #f)))

(define-checked (char-properties (object char?))
  `((char->integer ,(char->integer object))
    ,@(when/null (char-category object)
                 `((char-category ,(char-category object))))
    (char-alphabetic? ,(char-alphabetic? object))
    (char-numeric? ,(char-numeric? object))
    (char-whitespace? ,(char-whitespace? object))
    (char-lower-case? ,(char-lower-case? object))
    (char-upper-case? ,(char-upper-case? object))))

(define-checked (string-properties (object string?))
  `((string->symbol ,(string->symbol object))
    (string->utf8 ,(string->utf8 object))
    (string->number ,(string->number object))
    (string-length ,(string-length object))
    (string-byte-length
     ,(bytevector-length (string->utf8 object)))
    (file-exists? ,(file-exists? object))
    ,@(map (cut list <> <>)
           (iota (string-length object))
           (string->list object))))

(define-checked (vector-properties (object vector?))
  `((vector-length ,(vector-length object))
    ,@(map (cut list <> <>)
           (iota (vector-length object))
           (vector->list object))))

(define-checked (bytevector-properties (object bytevector?))
  `(,@(when/null (false-if-error (utf8->string object))
                 `((utf8->string ,(utf8->string object))))
    ,@(let loop ((idx 0))
        (if (>= idx (bytevector-length object))
            '()
            (cons (list idx (bytevector-u8-ref object idx))
                  (loop (+ 1 idx)))))))

(define-checked (port-properties (object port?))
  `((port-open? ,(port-open? object))
    (input-port? ,(input-port? object))
    (output-port? ,(output-port? object))
    (textual-port? ,(textual-port? object))
    (binary-port? ,(binary-port? object))
    (port-direction ,(cond
                      ((and (input-port? object)
                            (output-port? object))
                       'both)
                      ((input-port? object)
                       'input)
                      ((output-port? object)
                       'output)))
    (port-type ,(cond
                 ((port-fileno object)
                  'file)
                 ((textual-port? object)
                  'textual)
                 (else
                  'binary)))
    ,@(when/null (port-fileno object)
                 `((port-file-descriptor ,(port-fileno object))))
    (port-line ,(port-line object))
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-string object)))
                 `((get-output-string ,(get-output-string object))))
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-bytevector object)))
                 `((get-output-bytevector ,(get-output-bytevector object))))))

(define-checked (procedure-arity-191 object)
  (let ((arity (if (opcode? object)
                   (opcode-num-params object)
                   (procedure-arity object)))
        (variadic? (if (opcode? object)
                       (opcode-variadic? object)
                       (procedure-variadic? object))))
    (- (* (if variadic? -1 1)
          (if (zero? arity)
              1
              (expt 2 arity)))
       (if variadic? 1 0))))


;; Adapted from geiser-chibi <https://codeberg.org/geiser/chibi>
(define-checked (procedure-arglist (proc procedure?))
  (let ((analysis (false-if-error (procedure-analysis proc))))
    (if (lambda? analysis)
        (let ((arglist (lambda-params analysis)))
          (if (pair? arglist)
              (let loop ((arglist arglist))
                (cond ((null? arglist)
                       '())
                      ((symbol? arglist)
                       arglist)
                      (else
                       (cons (car arglist)
                             (loop (cdr arglist))))))
              #f))
        (if (procedure-arity proc)
            (let loop ((num (procedure-arity proc))
                       (var? (procedure-variadic? proc)))
              (cond
               ((and (zero? num) var?)
                '_)
               ((zero? num)
                '())
               (else
                (cons '_ (loop (- num 1) var?)))))
            #f))))

(define (gen-arglist object)
  (cond
   ((opcode? object)
    (let loop ((num (opcode-num-params object))
               (var? (opcode-variadic? object)))
      (cond
       ((and (zero? num) var?)
        '_)
       ((zero? num)
        '())
       (else
        (cons '_ (loop (- num 1) var?))))))
   ((procedure? object)
    (procedure-arglist object))
   (else #f)))

(define-checked (procedure-properties (object procedure?))
  `((procedure-name ,(if (opcode? object)
                         (string->symbol (opcode-name object))
                         (procedure-name object)))
    (procedure-arity-mask ,(procedure-arity-191 object))
    #;(procedure-arity ,(procedure-arity-102 object))
    ,@(when/null (gen-arglist object)
                 `((procedure-arglists ,(list (gen-arglist object)))))
    ,@(when/null (and (not (opcode? object))
                      (positive?
                       (vector-length
                        (bytecode-source
                         (procedure-code object)))))
                 (let ((source (vector-ref (bytecode-source
                                            (procedure-code object))
                                           0)))
                   (match source
                     ((_ file . line)
                      `((procedure-file ,file)
                        (procedure-line ,line))))))
    ,@(when/null (opcode? object)
                 `((procedure-argument-types
                    ,(let ((type (lambda (i)
                                   (type-name (or (opcode-param-type object i)
                                                  Object))))
                           (params (opcode-num-params object)))
                       (let loop ((param 0)
                                  (variadic? (opcode-variadic? object)))
                         (cond
                          ((and (= param params) variadic?)
                           (type (+ 1 params)))
                          ((= param params)
                           '())
                          (else (cons (type param)
                                      (loop (+ param 1) variadic?)))))))
                   (procedure-return-types ,(list (type-name (opcode-return-type object))))))
    ,@(when/null (procedure/tag? object)
                 `((procedure-tag ,(procedure-tag object))))))

(define (record-properties object)
  `((record-type ,(type-name (type-of object)))
    ,@(let ((slots (type-slots (type-of object))))
        (map (lambda (name idx)
               (list name (slot-ref (type-of object) object idx)))
             slots
             (iota (length slots))))))

(define-checked (type-properties (object type?))
  `((type-name ,(type-name object))
    ,@(when/null (type-parent object)
                 `((type-parent ,(type-parent object))))
    ,@(when/null (type-slots object)
                 `((type-slots ,(type-slots object))))))

(define-checked (error-object-properties (object error-object?))
  `((error-object-message ,(error-object-message object))
    (error-object-irritants ,(error-object-irritants object))))

(define-checked (exception-properties (object exception?))
  `((exception-message ,(exception-message object))
    (exception-irritants ,(exception-irritants object))))

(define-checked (hash-properties (object hash-table?))
  `((hash-table-equivalence-function
     ,(let ((eq (hash-table-equivalence-function object)))
        (if (opcode? eq)
            (string->symbol (opcode-name eq))
            (procedure-name eq))))
    (hash-table-hash-function
     ,(let ((eq (hash-table-hash-function object)))
        (if (opcode? eq)
            (string->symbol (opcode-name eq))
            (procedure-name eq))))
    (hash-table-size ,(hash-table-size object))
    ,@(map (lambda (entry)
             (list (car entry) (cdr entry)))
           (hash-table->alist object))))

(define-checked (numeric-vector-properties object)
  (let ((build (lambda (tag length-proc length-proc-name ref-proc)
                 `((vector-tag ,tag)
                   (,length-proc-name ,(length-proc object))
                   ,@(map (lambda (idx)
                            (list idx (ref-proc object idx)))
                          (iota (length-proc object)))))))
    (check-case
     object
     (s8vector?
      (build 's8 s8vector-length 's8vector-length s8vector-ref))
     (u8vector?
      (build 'u8 u8vector-length 'u8vector-length u8vector-ref))
     (s16vector?
      (build 's16 s16vector-length 's16vector-length s16vector-ref))
     (u16vector?
      (build 'u16 u16vector-length 'u16vector-length u16vector-ref))
     (s32vector?
      (build 's32 s32vector-length 's32vector-length s32vector-ref))
     (u32vector?
      (build 'u32 u32vector-length 'u32vector-length u32vector-ref))
     (s64vector?
      (build 's64 s64vector-length 's64vector-length s64vector-ref))
     (u64vector?
      (build 'u64 u64vector-length 'u64vector-length u64vector-ref))
     (f32vector?
      (build 'f32 f32vector-length 'f32vector-length f32vector-ref))
     (f64vector?
      (build 'f64 f64vector-length 'f64vector-length f64vector-ref))
     (c64vector?
      (build 'c64 c64vector-length 'c64vector-length c64vector-ref))
     (c128vector?
      (build 'c128 c128vector-length 'c128vector-length c128vector-ref)))))

(define-checked (char-set-properties (object char-set?))
  `((char-set-size ,(char-set-size object))
    (char-set-name ,(cond
                     ((eq? char-set:lower-case object)
                      'char-set:lower-case)
                     ((eq? object char-set:upper-case)
                      'char-set:upper-case)
                     ((eq? object char-set:title-case)
                      'char-set:title-case)
                     ((eq? object char-set:letter)
                      'char-set:letter)
                     ((eq? object char-set:digit)
                      'char-set:digit)
                     ((eq? object char-set:letter+digit)
                      'char-set:letter+digit)
                     ((eq? object char-set:graphic)
                      'char-set:graphic)
                     ((eq? object char-set:printing)
                      'char-set:printing)
                     ((eq? object char-set:whitespace)
                      'char-set:whitespace)
                     ((eq? object char-set:iso-control)
                      'char-set:iso-control)
                     ((eq? object char-set:punctuation)
                      'char-set:punctuation)
                     ((eq? object char-set:symbol)
                      'char-set:symbol)
                     ((eq? object char-set:hex-digit)
                      'char-set:hex-digit)
                     ((eq? object char-set:blank)
                      'char-set:blank)
                     ((eq? object char-set:ascii)
                      'char-set:ascii)
                     ((eq? object char-set:empty)
                      'char-set:empty)
                     ((eq? object char-set:full)
                      'char-set:full)))
    ,@(map (lambda (char) (list char char))
           (char-set->list object))))

(define (inspect-properties object)
  (append (object-properties object)
          ((check-case
            object
            (number? number-properties)
            (boolean? boolean-properties)
            (pair? pair-properties)
            (symbol? symbol-properties)
            (char? char-properties)
            (string? string-properties)
            (vector? vector-properties)
            (bytevector? bytevector-properties)
            (port? port-properties)
            (procedure? procedure-properties)
            (type? type-properties)
            (error-object? error-object-properties)
            (exception? exception-properties)
            (hash-table? hash-properties)
            (s8vector? numeric-vector-properties)
            (u8vector? numeric-vector-properties)
            (s16vector? numeric-vector-properties)
            (u16vector? numeric-vector-properties)
            (s32vector? numeric-vector-properties)
            (u32vector? numeric-vector-properties)
            (s64vector? numeric-vector-properties)
            (u64vector? numeric-vector-properties)
            (f32vector? numeric-vector-properties)
            (f64vector? numeric-vector-properties)
            (char-set? char-set-properties)
            (else record-properties))
           object)))

;;; inspect-describe

(define-checked (number-describe (object number?))
  (let ((props (number-properties object)))
    (check-case
     object
     (rational? (? "Number ") (? object)
                (and-let* ((len (assoc-ref 'integer-length props)))
                  (? " (") (? len) (? " bits)"))
                (? " #b") (? (assoc-ref 'display-2 props))
                (? " #o") (? (assoc-ref 'display-8 props))
                (? " #x") (? (assoc-ref 'display-16 props))
                (when (<= 0 object 1)
                  (? " ") (? (* object 100)) (? "%")))
     (else (? "Number ") (? object)))))

(define-checked (boolean-describe (object boolean?))
  (let ((props (boolean-properties object)))
    (? "Boolean ") (? (if object "#true" "#false"))
    (? " (") (? (assoc-ref 'boolean->integer props)) (? ")")))

(define-checked (pair-describe (object pair?))
  (let ((props (pair-properties object)))
    (? "Pair ")
    (if (circular-list? object)
        (write/ss object)
        (let ((first-five (take-5 object)))
          (? "(")
          (? (car first-five))
          (map (lambda (elem)
                 (? " ")
                 (? elem))
               (cdr first-five))
          (when (> (length+ object) 5)
            (? " [...and ") (? (- (length+ object) 5)) (? " more]"))
          (and-let* ((dl (assoc-ref 'dotted-last props)))
            (? " . ")
            (? dl))
          (? ")")))))

(define-checked (symbol-describe (object symbol?))
  (let ((props (symbol-properties object)))
    (? "Symbol ") (? object)
    (and-let* ((val (assoc-ref 'symbol-value props)))
      (? " = ") (? val))))

(define-checked (char-describe (object char?))
  (let ((props (char-properties object)))
    (? "Char ") (write object)
    (? " U+") (? (string-upcase (number->string (char->integer object))))
    (and-let* ((category (assoc-ref 'char-category props)))
      (? "[") (? category) (? "]"))))

(define-checked (string-describe (object string?))
  (let ((props (string-properties object)))
    (? "String \"") (map ? (take-10 (string->list object)))
    (when (> (string-length object) 10)
      (? "[...and ") (? (- (string-length object) 10)) (? " more]"))
    (? "\"")
    (and-let* ((len (assoc-ref 'string-byte-length props)))
      (? " (") (? len) (? " bytes)"))))

(define-checked (vector-describe (object vector?))
  (let ((props (vector-properties object)))
    (? "Vector #")
    (print-abridged (vector->list object))
    (when (assoc-ref 'vector->string props)
      (? " (") (write (assoc-ref 'vector->string props)) (? ")"))))

(define-checked (bytevector->list (object bytevector?)) => (list?)
  (let loop ((idx 0))
    (if (>= idx (bytevector-length object))
        '()
        (cons (bytevector-u8-ref object idx) (loop (+ 1 idx))))))

(define-checked (bytevector-describe (object bytevector?))
  (let ((props (bytevector-properties object)))
    (? "Bytevector #u8")
    (print-abridged (bytevector->list object))
    (when (assoc-ref 'utf8->string props)
      (? " (") (write (assoc-ref 'utf8->string props)) (? ")"))))

(define-checked (procedure-describe (object procedure?))
  (let ((props (procedure-properties object)))
    (begin (? "Procedure ")
           (? (or (assoc-ref 'procedure-name props)
                  "λ"))
           (and-let* ((arglists (assoc-ref 'procedure-arglists props)))
             (? " ")
             (? (fold-right (lambda (arglist acc)
                              (cons* arglist (string->symbol "|") acc))
                            (car arglists)
                            (cdr arglists))))
           (when (or (assoc-ref 'procedure-argument-types props)
                     (assoc-ref 'procedure-return-types props))
             (newline)
             (? (assoc-ref 'procedure-argument-types props))
             (? " → ")
             (? (assoc-ref 'procedure-return-types props))))))

(define-checked (type-describe (object type?))
  (let ((props (type-properties object)))
    (begin (? "Type ")
           (? (assoc-ref 'type-name props))
           (and-let* ((slots (assoc-ref 'type-slots props))
                      (slots? (pair? slots)))
             (? " ")
             (? "[")
             (? (car (assoc-ref 'type-slots props)))
             (unless (null? (cdr (assoc-ref 'type-slots props)))
               (map (lambda (x)
                      (? ", ")
                      (? x))
                    (cdr (assoc-ref 'type-slots props))))
             (? "]")))))

(define-checked (error-object-describe (object error-object?))
  (begin (? "Error object ")
         (write (error-object-message object))
         (? " ")
         (? (error-object-irritants object))))

(define-checked (exception-describe (object exception?))
  (begin (? "Exception ")
         (write (exception-message object))
         (? " ")
         (? (exception-irritants object))))

(define-checked (hash-describe (object hash-table?))
  (let ((props (hash-properties object)))
    (? "Hash table [")
    (? (assoc-ref 'hash-table-equivalence-function props))
    (? ", ")
    (? (assoc-ref 'hash-table-size props))
    (? "]\n")
    (map (lambda (pair)
           (? "  ") (? pair) (newline))
         (take-5 (hash-table->alist object)))))

(define-checked (port-describe (object port?))
  (let ((props (port-properties object)))
    (if (assoc-ref 'port-open? props)
        (? "Open ")
        (? "Closed "))
    (case (assoc-ref 'port-direction props)
      ((input) (? "Input "))
      ((output) (? "Output "))
      ((both) (? "Bidirectional ")))
    (? "port ")
    (? object)))

(define-checked (numeric-vector-describe object)
  (let ((describe (lambda (tag length-proc length-proc-name list-proc ref-proc)
                    (? (string-upcase (symbol->string tag)))
                    (? " Vector [")
                    (? (length-proc object))
                    (? "] ")
                    (? "#")
                    (? tag)
                    (print-abridged (list-proc object)))))
    (check-case
     object
     (s8vector?
      (describe 's8 s8vector-length 's8vector-length s8vector->list s8vector-ref))
     (u8vector?
      (describe 'u8 u8vector-length 'u8vector-length u8vector->list u8vector-ref))
     (s16vector?
      (describe 's16 s16vector-length 's16vector-length s16vector->list s16vector-ref))
     (u16vector?
      (describe 'u16 u16vector-length 'u16vector-length u16vector->list u16vector-ref))
     (s32vector?
      (describe 's32 s32vector-length 's32vector-length s32vector->list s32vector-ref))
     (u32vector?
      (describe 'u32 u32vector-length 'u32vector-length u32vector->list u32vector-ref))
     (s64vector?
      (describe 's64 s64vector-length 's64vector-length s64vector->list s64vector-ref))
     (u64vector?
      (describe 'u64 u64vector-length 'u64vector-length u64vector->list u64vector-ref))
     (f32vector?
      (describe 'f32 f32vector-length 'f32vector-length f32vector->list f32vector-ref))
     (f64vector?
      (describe 'f64 f64vector-length 'f64vector-length f64vector->list f64vector-ref)))))

(define-checked (char-set-describe (object char-set?))
  (let ((props (char-set-properties object)))
    (? "Char set ")
    (when (assoc-ref 'char-set-name props)
      (? (assoc-ref 'char-set-name props))
      (? " "))
    (? "{")
    (? (list->string (take-10 (char-set->list object))))
    (when (> (char-set-size object) 10)
      (? "..."))
    (? "}")))

(define-checked (record-describe object)
  (let ((props (record-properties object)))
    (? "Record ")
    (? (assoc-ref 'record-type props))
    (? " ")
    (? object)
    (newline)
    (map (lambda (prop)
           (? "  ")
           (? (car prop))
           (? ": ")
           (? (cadr prop))
           (newline))
         (cdr props))))

(define inspect-describe
  (case-lambda-checked
   ((object) (inspect-describe object (current-output-port)))
   ((object (port port?))
    (parameterize ((current-output-port port))
      ((check-case
        object
        (number? number-describe)
        (boolean? boolean-describe)
        (pair? pair-describe)
        (symbol? symbol-describe)
        (char? char-describe)
        (string? string-describe)
        (vector? vector-describe)
        (bytevector? bytevector-describe)
        (port? port-describe)
        (procedure? procedure-describe)
        (type? type-describe)
        (error-object? error-object-describe)
        (exception? exception-describe)
        (hash-table? hash-describe)
        (s8vector? numeric-vector-describe)
        (u8vector? numeric-vector-describe)
        (s16vector? numeric-vector-describe)
        (u16vector? numeric-vector-describe)
        (s32vector? numeric-vector-describe)
        (u32vector? numeric-vector-describe)
        (s64vector? numeric-vector-describe)
        (u64vector? numeric-vector-describe)
        (f32vector? numeric-vector-describe)
        (f64vector? numeric-vector-describe)
        (char-set? char-set-describe)
        (else record-describe))
       object)))))
