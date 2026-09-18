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

(import (scheme base)
        (scheme write)
        (scheme char)
        (scheme complex)
        (scheme file)
        (scheme eval)
        (scheme case-lambda))

(import (srfi 1)) ;; List library
(import (srfi 4))
(import (srfi 26))

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

(define (to-string-with object proc)
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
    (size ,(##sys#size object))
    (write ,(to-string-with object write))
    (display ,(to-string-with object display))))

(define (number-properties object)
  `((real-part ,(real-part object))
    (imag-part ,(imag-part object))
    ,@(when/null (rational? object)
                 `((numerator ,(numerator object))
                   (denominator ,(denominator object))
                   (real-sign ,(cond
                                ((negative? object) -1)
                                ((zero? object) 0)
                                ((positive? object) 1)))))
    (real-base 2)
    ;; Chibi encodes flonums as double-s
    (real-precision 53)
    ,@(when/null (and (integer? object)
                      (exact? object))
                 `((integer-length ,(integer-length object))))
    ,@(when/null (and (integer? object)
                      (exact? object)
                      (<= object #x10FFFF))
                 `((integer->char ,(integer->char object))))
    ;; integer-object seems to be generally impossible:
    ;; https://github.com/ashinn/chibi-scheme/issues/1136
    (display-2 ,(number->string object 2))
    (display-8 ,(number->string object 8))
    (display-16 ,(number->string object 16))))

(define (boolean-properties object)
  ;; sexp.h has these, but I daren’t go there:
  ;; #define SEXP_FALSE  SEXP_MAKE_IMMEDIATE(0) /* 14 0x0e */
  ;; #define SEXP_TRUE   SEXP_MAKE_IMMEDIATE(1) /* 30 0x1e */
  `((boolean->integer ,(if object 1 0))))

(define (pair-properties object)
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

(define (symbol-properties object)
  `((symbol->string ,(symbol->string object))
    (symbol-interned? ,(##sys#interned-symbol? object))
    (when/null
     (not (null? (##sys#slot object 2)))
     `((symbol-plist ,(##sys#slot object 2))))
    ;; That’s crude and likely quite broken, but whatever
    ,@(call/cc
       (lambda (k)
         (with-exception-handler
             (lambda (e)
               (k '()))
           (lambda _
             `((symbol-value ,(eval object (interaction-environment))))))))))

(define (char-category c)
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

(define (char-properties object)
  `((char->integer ,(char->integer object))
    ,@(when/null (char-category object)
                 `((char-category ,(char-category object))))
    (char-alphabetic? ,(char-alphabetic? object))
    (char-numeric? ,(char-numeric? object))
    (char-whitespace? ,(char-whitespace? object))
    (char-lower-case? ,(char-lower-case? object))
    (char-upper-case? ,(char-upper-case? object))))

(define (string-properties object)
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

(define (vector-properties object)
  `((vector-length ,(vector-length object))
    ,@(map (cut list <> <>)
           (iota (vector-length object))
           (vector->list object))))

(define (bytevector-properties object)
  `(,@(when/null (false-if-error (utf8->string object))
                 `((utf8->string ,(utf8->string object))))
    ,@(let loop ((idx 0))
        (if (>= idx (bytevector-length object))
            '()
            (cons (list idx (bytevector-u8-ref object idx))
                  (loop (+ 1 idx)))))))

(define (port-properties object)
  `((port-open? ,(if (input-port? object)
                     (input-port-open? object)
                     (output-port-open? object)))
    (input-port? ,(input-port? object))
    (output-port? ,(output-port? object))
    (textual-port? ,(textual-port? object))
    (binary-port? ,(binary-port? object))
    (port-encoding ,(##sys#slot object 15))
    (port-direction ,(cond
                      ((and (input-port? object)
                            (output-port? object))
                       'both)
                      ((input-port? object)
                       'input)
                      ((output-port? object)
                       'output)))
    (port-type ,(##sys#slot object 14))
    ,@(when/null (##sys#slot object 3)
                 `((port-file ,(##sys#slot object 3))))
    (port-line ,(##sys#slot object 4))
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-string object)))
                 `((get-output-string ,(get-output-string object))))
    ,@(when/null (and (output-port? object)
                      (false-if-error (get-output-bytevector object)))
                 `((get-output-bytevector ,(get-output-bytevector object))))))

(define (procedure-name proc)
  (let ((proc-info (procedure-information proc)))
    (car proc-info)))

(define (procedure-arity-191 object)
  (let* ((proc-info (procedure-information object))
         (arity (length+ (cdr proc-info)))
         (variadic? (dotted-list? proc-info)))
    (- (* (if variadic? -1 1)
          (if (zero? arity)
              1
              (expt 2 arity)))
       (if variadic? 1 0))))

(define (procedure-properties object)
  ;; TODO: type introspection
  (let ((proc-info (procedure-information object)))
    `((procedure-name ,(procedure-name object))
      (procedure-arity-mask ,(procedure-arity-191 object))
      (procedure-arglists ,(list (cdr proc-info))))))

(define (record-properties object)
  `((record-type ,(##sys#slot object 0))
    ,@(do ((idx 1 (+ 1 idx))
           (slots '()
                  (cons `(,(- idx 1) ,(##sys#slot object idx))
                        slots)))
          ((>= idx (##sys#size object)) (reverse! slots)))))

(define (error-object-properties object)
  `((error-object-message ,(error-object-message object))
    (error-object-irritants ,(error-object-irritants object))))

(define (hash-table->alist* object)
  (let* ((vec (##sys#slot object 1)))
    (fold append '()
          (map (lambda (i)
                 (map (lambda (bucket)
                        (list (##sys#slot bucket 0) (##sys#slot bucket 1)))
                      (##sys#slot vec i)))
               (iota (##sys#size vec))))))

(define (hash-properties object)
  `((hash-table-equivalence-function ,(procedure-name (##sys#slot object 3)))
    (hash-table-size ,(##sys#slot object 2))
    ,@(hash-table->alist* object)))

(define (numeric-vector-properties object)
  (let ((build (lambda (tag length-proc length-proc-name ref-proc)
                 `((vector-tag ,tag)
                   (,length-proc-name ,(length-proc object))
                   ,@(map (lambda (idx)
                            (list idx (ref-proc object idx)))
                          (iota (length-proc object)))))))
    (cond
     ((s8vector? object)
      (build 's8 s8vector-length 's8vector-length s8vector-ref))
     ((u8vector? object)
      (build 'u8 u8vector-length 'u8vector-length u8vector-ref))
     ((s16vector? object)
      (build 's16 s16vector-length 's16vector-length s16vector-ref))
     ((u16vector? object)
      (build 'u16 u16vector-length 'u16vector-length u16vector-ref))
     ((s32vector? object)
      (build 's32 s32vector-length 's32vector-length s32vector-ref))
     ((u32vector? object)
      (build 'u32 u32vector-length 'u32vector-length u32vector-ref))
     ((s64vector? object)
      (build 's64 s64vector-length 's64vector-length s64vector-ref))
     ((u64vector? object)
      (build 'u64 u64vector-length 'u64vector-length u64vector-ref))
     ((f32vector? object)
      (build 'f32 f32vector-length 'f32vector-length f32vector-ref))
     ((f64vector? object)
      (build 'f64 f64vector-length 'f64vector-length f64vector-ref)))))

(define (inspect-properties object)
  (append (object-properties object)
          ((cond
            ((number? object) number-properties)
            ((boolean? object) boolean-properties)
            ((pair? object) pair-properties)
            ((symbol? object) symbol-properties)
            ((char? object) char-properties)
            ((string? object) string-properties)
            ((vector? object) vector-properties)
            ((bytevector? object) bytevector-properties)
            ((port? object) port-properties)
            ((procedure? object) procedure-properties)
            ((error-object? object) error-object-properties)
            ((##sys#structure? object 'hash-table)
             hash-properties)
            ((or (s8vector? object)
                 (u8vector? object)
                 (s16vector? object)
                 (u16vector? object)
                 (s32vector? object)
                 (u32vector? object)
                 (s64vector? object)
                 (u64vector? object)
                 (f32vector? object)
                 (f64vector? object))
             numeric-vector-properties)
            (else record-properties))
           object)))

;;; inspect-property

(define (inspect-property object key)
  (let ((pair (assoc key (inspect-properties object))))
    (if pair
        (values (cadr pair) #t)
        (values #f #f))))

;;; inspect-describe

(define (number-describe object)
  (let ((props (number-properties object)))
    (cond
     ((rational? object)
      (? "Number ") (? object)
      (and-let* ((len (assoc-ref 'integer-length props)))
        (? " (") (? len) (? " bits)"))
      (? " #b") (? (assoc-ref 'display-2 props))
      (? " #o") (? (assoc-ref 'display-8 props))
      (? " #x") (? (assoc-ref 'display-16 props))
      (when (<= 0 object 1)
        (? " ") (? (* object 100)) (? "%")))
     (else (? "Number ") (? object)))))

(define (boolean-describe object)
  (let ((props (boolean-properties object)))
    (? "Boolean ") (? (if object "#true" "#false"))
    (? " (") (? (assoc-ref 'boolean->integer props)) (? ")")))

(define (pair-describe object)
  (let ((props (pair-properties object)))
    (? "Pair ")
    (if (circular-list? object)
        (? "(circular)")
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

(define (symbol-describe object)
  (let ((props (symbol-properties object)))
    (? "Symbol ") (? object)
    (and-let* ((val (assoc-ref 'symbol-value props)))
      (? " = ") (? val))))

(define (char-describe object)
  (let ((props (char-properties object)))
    (? "Char ") (write object)
    (? " U+") (? (string-upcase (number->string (char->integer object) 16)))
    (and-let* ((category (assoc-ref 'char-category props)))
      (? "[") (? category) (? "]"))))

(define (string-describe object)
  (let ((props (string-properties object)))
    (? "String \"") (map ? (take-10 (string->list object)))
    (when (> (string-length object) 10)
      (? "[...and ") (? (- (string-length object) 10)) (? " more]"))
    (? "\"")
    (and-let* ((len (assoc-ref 'string-byte-length props)))
      (? " (") (? len) (? " bytes)"))))

(define (vector-describe object)
  (let ((props (vector-properties object)))
    (? "Vector #")
    (print-abridged (vector->list object))
    (when (assoc-ref 'vector->string props)
      (? " (") (write (assoc-ref 'vector->string props)) (? ")"))))

(define (bytevector->list object)
  (let loop ((idx 0))
    (if (>= idx (bytevector-length object))
        '()
        (cons (bytevector-u8-ref object idx) (loop (+ 1 idx))))))

(define (bytevector-describe object)
  (let ((props (bytevector-properties object)))
    (? "Bytevector #u8")
    (print-abridged (bytevector->list object))
    (when (assoc-ref 'utf8->string props)
      (? " (") (write (assoc-ref 'utf8->string props)) (? ")"))))

(define (procedure-describe object)
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

(define (error-object-describe object)
  (begin (? "Error object ")
         (write (error-object-message object))
         (? " ")
         (? (error-object-irritants object))))

(define (hash-describe object)
  (let ((props (hash-properties object)))
    (? "Hash table [")
    (? (assoc-ref 'hash-table-equivalence-function props))
    (? ", ")
    (? (assoc-ref 'hash-table-size props))
    (? "]\n")
    (map (lambda (pair)
           (? "  ") (? pair) (newline))
         (take-5 (hash-table->alist* object)))))

(define (port-describe object)
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

(define (numeric-vector-describe object)
  (let ((describe (lambda (tag length-proc length-proc-name list-proc ref-proc)
                    (? (string-upcase (symbol->string tag)))
                    (? " Vector [")
                    (? (length-proc object))
                    (? "] ")
                    (? "#")
                    (? tag)
                    (print-abridged (list-proc object)))))
    (cond
     ((s8vector? object)
      (describe 's8 s8vector-length 's8vector-length s8vector->list s8vector-ref))
     ((u8vector? object)
      (describe 'u8 u8vector-length 'u8vector-length u8vector->list u8vector-ref))
     ((s16vector? object)
      (describe 's16 s16vector-length 's16vector-length s16vector->list s16vector-ref))
     ((u16vector? object)
      (describe 'u16 u16vector-length 'u16vector-length u16vector->list u16vector-ref))
     ((s32vector? object)
      (describe 's32 s32vector-length 's32vector-length s32vector->list s32vector-ref))
     ((u32vector? object)
      (describe 'u32 u32vector-length 'u32vector-length u32vector->list u32vector-ref))
     ((s64vector? object)
      (describe 's64 s64vector-length 's64vector-length s64vector->list s64vector-ref))
     ((u64vector? object)
      (describe 'u64 u64vector-length 'u64vector-length u64vector->list u64vector-ref))
     ((f32vector? object)
      (describe 'f32 f32vector-length 'f32vector-length f32vector->list f32vector-ref))
     ((f64vector? object)
      (describe 'f64 f64vector-length 'f64vector-length f64vector->list f64vector-ref)))))

(define (record-describe object)
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
  (case-lambda
   ((object) (inspect-describe object (current-output-port)))
   ((object port)
    (parameterize ((current-output-port port))
      ((cond
        ((number? object) number-describe)
        ((boolean? object) boolean-describe)
        ((pair? object) pair-describe)
        ((symbol? object) symbol-describe)
        ((char? object) char-describe)
        ((string? object) string-describe)
        ((vector? object) vector-describe)
        ((bytevector? object) bytevector-describe)
        ((port? object) port-describe)
        ((procedure? object) procedure-describe)
        ((error-object? object) error-object-describe)
        ((##sys#structure? object 'hash-table) hash-describe)
        ((s8vector? object) numeric-vector-describe)
        ((u8vector? object) numeric-vector-describe)
        ((s16vector? object) numeric-vector-describe)
        ((u16vector? object) numeric-vector-describe)
        ((s32vector? object) numeric-vector-describe)
        ((u32vector? object) numeric-vector-describe)
        ((s64vector? object) numeric-vector-describe)
        ((u64vector? object) numeric-vector-describe)
        ((f32vector? object) numeric-vector-describe)
        ((f64vector? object) numeric-vector-describe)
        (else record-describe))
       object)))))
