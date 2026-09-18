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

(define-library (srfi 279)
  (export inspect-properties inspect-property inspect-describe)
  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme complex)
          (scheme file)
          (scheme eval)
          (scheme case-lambda))
  (import (srfi 253))
  (cond-expand
    (chibi
     (import (srfi 1)) ;; List library
     (import (srfi 2)) ;; and-let*
     (import (srfi 14)) ;; Char sets
     (import (srfi 26)) ;; cut/cute
     (import (srfi 69)) ;; Hash tables
     (import (srfi 151)) ;; Integers as bits
     (import (srfi 160 base)) ;; Numeric vectors
     (import (srfi 229)) ;; Tagged procedures
     (import (chibi))
     (import (chibi ast))
     (import (chibi modules))
     (import (chibi match))
     (include "chibi.scm"))
    (stklos
     (import (srfi 1)) ;; List library
     (import (srfi 2)) ;; and-let*
     (import (srfi 14)) ;; Char sets
     (import (srfi 18)) ;; Multithreading
     (import (srfi 19)) ;; Time
     (import (srfi 25)) ;; Multi-dimensional arrays
     (import (srfi 26)) ;; cut/cute
     (import (srfi 69)) ;; Hash tables
     (import (srfi 111)) ;; Boxes
     (import (srfi 113)) ;; Sets and bags
     (import (srfi 151)) ;; Integers as bits
     (import (srfi 160 base)) ;; Numeric vectors
     (import (srfi 195)) ;; Multiple-value boxes
     (import (srfi 229)) ;; Tagged procedures
     (include "stklos.scm"))
    (kawa
     (import (srfi 1))  ;; List library
     (import (srfi 8))  ;; receive
     (import (srfi 14)) ;; Char sets
     (import (srfi 26)) ;; cut/cute
     (import (srfi 60)) ;; Integers as bits
     (import (srfi 69)) ;; Hash tables
     (import (kawa lib reflection))
     (import (kawa lib ports))
     (include "kawa.scm"))
    (guile
     (import (srfi 1)) ;; List library
     (import (srfi 2)) ;; and-let*
     (import (scheme base))
     (import (scheme eval))
     (import (srfi 14)) ;; Char sets
     (import (srfi 26)) ;; cut/cute
     (import (srfi 60)) ;; Integers as bits
     (import (srfi 69)) ;; Hash tables
     (import (rnrs records inspection))
     (import (rnrs arithmetic fixnums))
     (import (rnrs arithmetic flonums))
     (import (rnrs arithmetic bitwise))
     (import (ice-9 match))
     (include "guile.scm"))
    (chicken
     (import (chicken base))
     (import (srfi 1)) ;; List library
     (import (srfi 4)) ;; Numeric vectors
     (import (srfi 26)) ;; cut / cute
     (include "chicken.scm"))
    (else
     (import (srfi 1))   ;; List library
     (import (srfi 14))  ;; Char sets
     (import (srfi 69))  ;; Hash tables
     (import (srfi 160)) ;; Numeric vectors
     (include "generic.scm"))))
