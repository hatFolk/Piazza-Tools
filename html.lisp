;; A translation of HTML tags to Common Lisp.
; Explanation of Param names:
; 	name := Name of the tag
;	entry/str := A string that is being manipulated to be incorporated to be tagged.
; 	css := CSS oneliners for insertion into the style attribute
;	attribs := Valid HTML additions
;; Notes to self: Determine if incorporation of all Global Attributes necessary.

(defmacro new-tag (name)
  (let ((entry-name (gensym))
		(css-name (gensym))
		(attribs-name (gensym)))
	`(defun ,(intern (string-upcase name)) (,entry-name &optional ,css-name ,attribs-name)
	   (tag-template ,name ,entry-name ,css-name ,attribs-name))))

(defun tag-template (name entry &optional css attribs)
  "Makes all sorts of tags. style is either nil or a valid string of css."
  (cond ((and (not css) (not attribs))(format nil "<~A>~A<\\~A>" name entry name))
		((and (not css) attribs)
		 (format nil "<~A ~A>~A<\\~A>" name attribs entry name))
		((and css (not attribs))
		 (format nil "<~A style=\"~A\">~A<\\~A>" name css entry name))
		(t    (format nil "<~A ~A style=\"~A\">~A<\\~A>" name attribs css entry name))))

(defun td (entry &optional css attribs)
  "Generates <td> entries with styles." 
  (tag-template  "td" entry css attribs )) 

(defun tr (entry &optional css attribs)
  "Generates <tr> entries with styles"
  (tag-template  "tr" entry css attribs ))

(defun table (entry &optional css attribs)
  "Generates <table> entries with styles"
  (tag-template  "table" entry css attribs))

(defun a (entry &optional css attribs)
  "Generates a tags. attribs is a string containing valid html attributes like href, hreflang, etc."
  (tag-template  "a" entry css attribs))

(defun simple-url (url entry &optional css)
  (a entry css (format nil "href=\"~A\"" url)))

(defun b (text &optional css attribs)
  (tag-template "b" text css attribs))

(defun i (text &optional css attribs)
  (tag-template "i" text css attribs))

(defun sup (text &optional css attribs)
  (tag-template "sup" text css attribs))

(defun sub (text &optional css attribs)
  (tag-template "sub" text css attribs))

(defun span (text &optional css attribs)
  (tag-template "span" text css attribs))

(defun underline (text &optional attribs)
  (span text "text-decoration: underline;" attribs))

(defun strikethrough (text &optional attribs)
  (span text "text-decoration: line-through;" attribs))

(defun li (x &optional css attribs)
  (tag-template "li" x css attribs))

(defun render-html () "...")

(defun ul (xs &key css attribs)
  (tag-template "ul" (reduce #'concatenate (map 'string #'(lambda (x) (li x)) xs)) css attribs))

(defun ol (xs &optional css attribs)
  (tag-template "ol" (reduce #'concatenate (map 'string #'(lambda (x) (li x)) xs)) css attribs))

(defun pre (str &optional css attribs)
  (tag-template "pre" str css attribs))

(defun p (str &optional css attribs)
  (tag-template "p" str css attribs))

(defun str-concat (&rest xs)
  )
