(define (select fields table-name . constraints)
  (format-rows (select-fields fields table-name constraints)))

(define (select-fields fields table-name constraints)
  (if (null? constraints)
      (filter-fields fields (table-select-all table-name))
      (filter-fields fields (filter-rows constraints (table-select-all table-name)))))
      
(define (filter-rows constraints rows)
  (if (null? constraints) rows
      (filter-rows (cdr constraints) (filter-constraint (car constraints) rows))))

(define (filter-constraint constraint rows)
  (filter (lambda (row) (check-constraint constraint row)) rows))

(define (check-constraint constraint cells)
  (define op (car constraint))
  (define field-name (cadr constraint))
  (define value (car (cddr constraint)))
  (cond ((eq? op 'eq) (eq? (cadr (assq field-name cells)) value))
        (else #f)))

(define (filter-fields fields rows)
  (define (field-ok f) (memq (car f) fields))
  (define (row-filter row) (filter field-ok row))
  (if (eq? fields 'all) rows
      (map row-filter rows)))

(define (spaces n)
  (if (<= n 0) "" (string-append " " (spaces (- n 1)))))

(define (write-column text width align-right)
  (define space-count (- width (string-length text)))
  (define gap (spaces space-count))
  (if align-right (write-string gap))
  (write-string text)
  (if (not align-right) (write-string gap))
  (write-string " "))

(define (for f xs)
  (if (null? xs) '()
      (begin
        (f (car xs))
        (for f (cdr xs)))))

(define (transpose xs) (transpose-join (car xs) (cdr xs)))

(define (transpose-join row cols)
  (if (null? row) nil
      (cons (cons (car row) (map car cols)) (transpose-join (cdr row) (map cdr cols)))))

(define (max-width col)
  (if (null? (cdr col)) (car col)
      (let ((r (max-width (cdr col)))) (if (> r (car col)) r (car col)))))

(define (zip xs ys)
  (if (null? xs) xs
      (if (null? ys) ys
          (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))

(define (format-rows rows)
  (define headers (map car (car rows)))
  (define value-rows (map (lambda (x) (map cadr x)) rows))
  (define cell-widths (map (lambda (x) (map string-length (map symbol->string x))) value-rows))
  (define header-widths (map string-length (map symbol->string headers)))
  (define col-widths (map max-width (transpose (cons header-widths cell-widths))))
  (define (show-row row)
    (for (lambda (x) (write-column (symbol->string (car x)) (cdr x) (number? (car x)))) row) (newline))
  (show-row (zip headers col-widths))
  (define rows-widths (map (lambda (x) (zip x col-widths)) value-rows))
  (for show-row rows-widths)
  (length value-rows))
