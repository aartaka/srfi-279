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
  (take lst (min (length lst) 5)))

(define (take-10 lst)
  (take lst (min (length lst) 10)))

(define-checked (procedure-arg-num (proc procedure?))
  (let* ((numArgs (proc:numArgs))
         (rest? (negative? numArgs)))
    (values (logand numArgs #b11111111111) rest?)))

(define-checked (%procedure-arg-types (proc procedure?))
  (let-values (((num rest?) (procedure-arg-num proc)))
    (let rec ((param 0))
      (cond
       ((and (= param num) rest?)
        (proc:getParameterType param))
       ((= param num)
        '())
       (else
        (cons (proc:getParameterType param)
              (rec (+ 1 param))))))))

(define (prettify-type type)
  (let* ((name ((type:toString):replace "ClassType " ""))
         (name (name:replace "Type " "")))
    ;; (cond
    ;;  ((equal? name "gnu.mapping.Symbol") 'symbol?)
    ;;  ((equal? name "gnu.expr.Keyword") 'keyword?)
    ;;  ((equal? name "list") 'list?)
    ;;  ;; TODO: Pair?
    ;;  ((equal? name "java.lang.CharSequence") 'string?)
    ;;  ((equal? name "character") 'character?)
    ;;  ((equal? name "vector") 'vector?)
    ;;  ((equal? name "gnu.mapping.Procedure") 'procedure?)
    ;;  ((equal? name "java.io.Reader") 'input-port?)
    ;;  ((equal? name "java.io.Writer") 'input-port?)
    ;;  ((equal? name "gnu.lists.Array") 'array?)
    ;;  ((equal? name "java.lang.Number") 'number?)
    ;;  ((equal? name "java.io.Closeable") 'port?)
    ;;  ((equal? name "gnu.math.Complex") 'complex?)
    ;;  ((equal? name "gnu.math.Quantity") 'quantity?)
    ;;  ((member name '("real" "rational" "integer"
    ;;                  "long" "int" "short" "byte"
    ;;                  "ulong" "uint" "ushort" "ubyte"
    ;;                  "double" "float"))
    ;;   (string->symbol (string-append name "?")))
    ;;  (else #f))
    name))

(define-checked (procedure-arg-types (proc procedure?))
  (case (if proc:name
            (string->symbol proc:name)
            #f)
    ((+ * - /)
     '())
    ((apply)
     '("gnu.mapping.Procedure"))
    ((array-ref)
     '("gnu.lists.Array"))
    ((array-set!)
     '("gnu.lists.Array"))
    ((bitwise-and bitwise-ior bitwise-xor)
     '())
    ((bitwise-arithmetic-shift bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right)
     '("integer" "integer"))
    ((bitwise-not) '("integer"))
    ((call-with-current-continuation call/cc)
     '("gnu.mapping.Procedure"))
    ((call-with-values)
     '("gnu.mapping.Procedure" "gnu.mapping.Procedure"))
    ((format)
     '(#f))
    ((floor/ floor-quotient floor-remainder
             truncate/ truncate-quotient truncate-remainder
             quotinent remainder
             div mod modulo div0 mod0)
     '("integer" "integer"))
    ((expt)
     '("gnu.math.Complex" "gnu.math.Complex"))
    ((eq? eqv? equal?)
     '(#f #f))
    ((list)
     '())
    ((make-procedure)
     '())
    ((map for-each)
     '("gnu.mapping.Procedure" "list"))
    ((> = < >= <=)
     '("java.lang.Number" "java.lang.Number"))
    ((run-process)
     '())
    ((even? odd?)
     '("integer"))
    (else
     (let ((types (%procedure-arg-types proc)))
       (let rec ((types types))
         (cond
          ((pair? types)
           (cons (prettify-type (car types))
                 (rec (cdr types))))
          ((null? types)
           '())
          (else (prettify-type types))))))))

(define-checked (procedure-name (proc procedure?))
  (string->symbol proc:name))

(define-syntax when/null
  (syntax-rules ()
    ((_ cond body ...)
     (if cond
         (begin body ...)
         (list)))))

(define-checked (to-string-with object (proc procedure?)) => (string?)
  (call-with-port (open-output-string)
    (lambda (p)
      (proc object p)
      (get-output-string p))))

;;; inspect-properties

(define (object-properties object)
  ;; hash-by-identity should be fine:
  ;; https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#hashCode--
  ;; As much as is reasonably practical, the hashCode method defined
  ;; by class Object does return distinct integers for distinct
  ;; objects. (This is typically implemented by converting the
  ;; internal address of the object into an integer, but this
  ;; implementation technique is not required by the Java™ programming
  ;; language.)
  `((id ,(hash-by-identity object))
    (location ,(hash-by-identity object))
    (type ,(prettify-type (object:getClass)))
    (write ,(to-string-with object write))
    (display ,(to-string-with object display))))

(define-checked (number-properties (object number?))
  `((real-part ,(real-part object))
    (imag-part ,(imag-part object))
    (numerator ,(numerator object))
    (denominator ,(denominator object))
    ,@(when/null (rational? object)
                 `((real-sign ,(check-case
                                object
                                (negative? -1)
                                (zero? 0)
                                (positive? 1)))))
    (real-base 2)
    ;; Kawa encodes flonums as doubles
    (real-precision 53)
    ,@(when/null (and (integer? object)
                      (<= object #x10FFFF))
                 `((integer->char ,(integer->char object))))
    (display-2 ,(number->string object 2))
    (display-8 ,(number->string object 8))
    (display-16 ,(number->string object 16))))

(define-checked (boolean-properties (object boolean?))
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
                   `((dotted-last ,truly-last)))
      ,@(when/null (not (or (circular-list? object)
                            (dotted-list? object)))
                   `((list->vector ,(list->vector object))))
      ,@(when/null (and (not (or (circular-list? object)
                                 (dotted-list? object)))
                        (every char? object))
                   `((list->string ,(list->string object)))))))

(define-checked (symbol-properties (object symbol?))
  `((symbol->string ,(symbol->string object))
    ;; That’s crude and likely quite broken, but whatever
    ,@(call/cc
       (lambda (k)
         (with-exception-handler
             (lambda (e)
               (k '()))
           (lambda _
             `((symbol-value ,(eval object)))))))))

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
    (string->list ,(string->list object))
    (string->vector ,(string->vector object))
    (string->utf8 ,(string->utf8 object))
    (string->number ,(string->number object))
    (string-length ,(string-length object))
    (file-exists? ,(file-exists? object))
    ,@(map (cut list <> <>)
           (iota (string-length object))
           (string->list object))))

(define-checked (vector-properties (object vector?))
  `((vector-length ,(vector-length object))
    (vector->list ,(vector->list object))
    ,@(when/null (every char? (vector->list object))
                 `((vector->string ,(vector->string object))))
    ,@(map (cut list <> <>)
           (iota (vector-length object))
           (vector->list object))))

(define-checked (port-properties (object port?))
  `((port-open? ,(if (input-port? object)
                     (input-port-open? object)
                     (output-port-open? object)))
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
    (port-type ,(if (textual-port? object)
                    'textual
                    'binary))
    ,@(when/null (input-port? object)
                 `((port-line ,(port-line object))
                   (port-column ,(port-column object))))
    #;
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-string object)))
                 `((get-output-string ,(get-output-string object))))
    #;
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-bytevector object)))
                 `((get-output-bytevector ,(get-output-bytevector object))))))

(define-checked (procedure-arity-191 object)
  (receive (arity variadic?)
      (procedure-arg-num object)
    (- (* (if variadic? -1 1)
          (if (zero? arity)
              1
              (expt 2 arity)))
       (if variadic? 1 0))))


;; Adapted from geiser-chibi <https://codeberg.org/geiser/chibi>
(define-checked (procedure-arglist (proc procedure?))
  (receive (arity variadic?)
      (procedure-arg-num object)
    (let loop ((num arity)
               (var? variadic?))
      (cond
       ((and (zero? num) var?)
        '_)
       ((zero? num)
        '())
       (else
        (cons '_ (loop (- num 1) var?)))))))

(define-checked (procedure-properties (object procedure?))
  `((procedure-name ,(procedure-name object))
    (procedure-arity-mask ,(procedure-arity-191 object))
    ,@(when/null (procedure-arglist object)
                 `((procedure-arglists ,(list (procedure-arglist object)))))
    ;; ,@(when/null (and (not (opcode? object))
    ;;                   (positive?
    ;;                    (vector-length
    ;;                     (bytecode-source
    ;;                      (procedure-code object)))))
    ;;              (let ((source (vector-ref (bytecode-source
    ;;                                         (procedure-code object))
    ;;                                        0)))
    ;;                (match source
    ;;                  ((x file . line)
    ;;                   `((procedure-file ,file)
    ;;                     (procedure-line ,line))))))
    ,@(when/null (procedure-arg-types object)
                 `((procedure-argument-types ,(procedure-arg-types object))))))

(define (record-properties object)
  `((record-type ,(object:getClass))
    ,@(let ((rtd (record-type-descriptor object)))
        (map (lambda (name)
               (list name ((record-accessor rtd name) object)))
             (record-type-field-names rtd)))))

(define-checked (error-object-properties (object error-object?))
  `((error-object-message ,(error-object-message object))
    (error-object-irritants ,(error-object-irritants object))))

(define-checked (hash-properties (object hash-table?))
  `((hash-table-equivalence-function
     ,(let ((eq (hash-table-equivalence-function object)))
        (string->symbol eq:name)))
    (hash-table-hash-function
     ,(let ((hsh (hash-table-hash-function object)))
        (string->symbol hsh:name)))
    (hash-table-size ,(hash-table-size object))
    ,@(map (lambda (entry)
             (list (car entry) (cdr entry)))
           (hash-table->alist object))))

(define-checked (numeric-vector-properties object)
  (check-case
   object
   (s8vector?
    `((vector-tag s8)
      (s8vector-length ,(length object))
      (s8vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (u8vector?
    `((vector-tag u8)
      (u8vector-length ,(length object))
      (u8vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (s16vector?
    `((vector-tag s16)
      (s16vector-length ,(length object))
      (s16vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (u16vector?
    `((vector-tag u16)
      (u16vector-length ,(length object))
      (u16vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (s32vector?
    `((vector-tag s32)
      (s32vector-length ,(length object))
      (s32vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (u32vector?
    `((vector-tag u32)
      (u32vector-length ,(length object))
      (u32vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (s64vector?
    `((vector-tag s64)
      (s64vector-length ,(length object))
      (s64vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (u64vector?
    `((vector-tag u64)
      (u64vector-length ,(length object))
      (u64vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (f32vector?
    `((vector-tag f32)
      (f32vector-length ,(length object))
      (f32vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))
   (f64vector?
    `((vector-tag f64)
      (f64vector-length ,(length object))
      (f64vector->list ,(->list object))
      ,@(map (lambda (idx)
               (list idx (vector-ref object idx)))
             (iota (length object)))))))

(define-checked (char-set-properties (object char-set?))
  `((char-set-size ,(char-set-size object))
    (char-set->list ,(char-set->list object))
    (char-set->string ,(char-set->string object))
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
            (port? port-properties)
            (procedure? procedure-properties)
            (error-object? error-object-properties)
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
            (record? record-properties))
           object)))

;;; inspect-describe

(define-checked (number-describe (object number?))
  (let ((props (number-properties object)))
    (check-case
     object
     (rational? (? "Number ") (? object)
                (when (assoc-ref 'integer-length props)
                  (? " (") (? (assoc-ref 'integer-length props)) (? " bits)"))
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

(define-checked (symbol-describe (object symbol?))
  (let ((props (symbol-properties object)))
    (? "Symbol ") (? object)
    (when (assoc-ref 'symbol-value props)
      (? " = ") (? (assoc-ref 'symbol-value props)))))

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
    (when (assoc-ref 'string-byte-length props)
      (? " (") (? (assoc-ref 'string-byte-length props)) (? " bytes)"))))

(define-checked (vector-describe (object vector?))
  (let ((props (vector-properties object)))
    (? "Vector #")
    (if (> (vector-length object) 5)
        (begin (? "(")
               (map (lambda (e)
                      (? e) (? " "))
                    (take-5 (vector->list object)))
               (when (> (vector-length object) 5)
                 (? "[...and ") (? (- (vector-length object) 5)) (? " more]"))
               (? ")"))
        (? (vector->list object)))
    (when (assoc-ref 'vector->string props)
      (? " (") (write (assoc-ref 'vector->string props)) (? ")"))))

(define inspect-describe
  (case-lambda-checked
   ((object) (inspect-describe object (current-output-port)))
   ((object (port port?))
    (parameterize ((current-output-port port))
      ((check-case
        object
        (number? number-describe)
        (boolean? boolean-describe)
        (pair? pair-properties)
        (symbol? symbol-describe)
        (char? char-describe)
        (string? string-describe)
        (vector? vector-describe)
        (procedure? procedure-properties)
        (error-object? error-object-properties)
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
       object port)))))
