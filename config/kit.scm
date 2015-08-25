(define (select-fields fields table-name constraints)
  (if (null? constraints)
      (filter-fields fields (table-select-all table-name))
      (filter-fields fields (table-select-all table-name))))
      
(define (select fields table-name key-values)
  (format-rows (select-fields fields table-name key-values)))
      
(define (filter-fields fields rows)
  (define (field-ok f) (memq (car f) fields))
  (define (row-filter row) (filter field-ok row))
  (map row-filter rows))

(define (spaces n)
  (if (<= n 0) "" (string-append " " (spaces (- n 1)))))

(define (write-column text width)
  (write-string (spaces (- 8 (string-length text))))
  (write-string text))

(define (for f xs)
  (if (null? xs) '()
      (begin
        (f (car xs))
        (for f (cdr xs)))))

(define (format-rows rows)
  (define headers (map car (car rows)))
  (define value-rows (map (lambda (x) (map cadr x)) rows))
  (define (show-row row) (for (lambda (x) (write-column (symbol->string x) 8)) row) (newline))
  (show-row headers)
  (for show-row value-rows)
  (length value-rows))
