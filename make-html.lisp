(defun table-entry (xs) ; Generates <td><tr> tags 
  (labels ; Every two entries is alignment and text.
	  ((recur (xs str)
		 (if xs
			 (recur (cdr (cdr xs))
					(format nil "~A<td style=\"text-align: ~A;\"~A<\\td>" str (car xs) (cdr (car xs))))
			 (format nil "~A<//tr>" str))
))
	(recur xs "<tr>")
))
y