(define-module debwatch
  (use rfc.ftp)
  (use file.util)
  (use gauche.collection)
  (use gauche.version)
  (use text.tr)
  (use rfc.uri)
  (export watch-file-url-for
	  default-host
	  default-main-dir))

(select-module debwatch)

(define (find-dhash-dir conn main-dir dpkg)
  (let* ((dir-list (map sys-basename (ftp-name-list conn main-dir)))
	 (regexp-list (map
		       (compose 
			string->regexp
			(pa$ format "^~a.*")
			regexp-quote)
		       dir-list))
	 (dhash-dir (fold (lambda (d r kdr)
			    (if (r dpkg) 
				(cond
				 ((not kdr) d)
				 ((> (string-length d) 
				     (string-length kdr))
				  d)
				 (else kdr))
				kdr))
			  #f
			  dir-list
			  regexp-list)))
    (if dhash-dir
	(build-path main-dir dhash-dir)
	(errorf "cannot find directory-hashing directory for ~a"
		dpkg))))

(define (find-pkg-dir conn dhash-dir dpkg)
  (let1 pkg-dir (build-path dhash-dir dpkg)
    (guard (e
	    (((with-module rfc.ftp <ftp-error>) e)
	     (raise e)))
      (ftp-chdir conn pkg-dir))
    (if pkg-dir
	pkg-dir
	(errorf "cannot find package directory for ~a" dpkg))))

(define (find-diff-gz-list conn pkg-dir dpkg)
  (let1 diff-gz-list (filter (lambda (elt)
			       (and ((string->regexp
				      (format "~a_.*\.diff\.gz$" (regexp-quote dpkg)))
				     elt)
				    elt))
			     (ftp-name-list conn pkg-dir))
    (if (null? diff-gz-list)
	(errorf "package directory for ~a is empty"  dpkg)
	diff-gz-list)))

(define (find-diff-gz diff-gz-list)
  (let1 diff-gz (find-max 
		 diff-gz-list 
		 :key (lambda (elt) (string-tr (sys-basename elt) "+" "-"))
		 ;; TODO: dpkg --compare-versions 9.7.2 lt 9.7.2-b1
		 :compare version<?)
    diff-gz))


(define default-host "ftp.debian.org")
(define default-main-dir "/debian/pool/main")

(define (watch-file-url-for host main-dir dpkg )
  (let1 host (or host default-host)
    (call-with-ftp-connection 
     host
     (lambda (conn)
       (let* ((dhash-dir (find-dhash-dir conn (or main-dir default-main-dir) dpkg))
	      (pkg-dir (find-pkg-dir conn dhash-dir dpkg))
	      (diff-gz-list (find-diff-gz-list conn pkg-dir dpkg))
	      (diff-gz (find-diff-gz diff-gz-list)))
	 (uri-compose :scheme "ftp" 
		      :host host
		      :path diff-gz)))
     :passive #t)))

(provide "debwatch")
