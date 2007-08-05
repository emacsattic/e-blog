(require 'xml)

(defvar e-blog-display-url nil
  "If non-nil, e-blog will display the post/edit url in post/edit
buffers.")

(setq e-blog-name "eblog"
      e-blog-version "0.4"
      e-blog-service "blogger"
      e-blog-fetch-authinfo-url "https://www.google.com/accounts/ClientLogin"
      e-blog-fetch-bloglist-url "http://www.blogger.com/feeds/default/blogs"
      e-blog-post-url-rel "http://schemas.google.com/g/2005#post"
      e-blog-buffer "*e-blog*"
      e-blog-post-buffer "*e-blog post*"
      e-blog-choose-buffer "*e-blog choose*"
      e-blog-edit-buffer "*e-blog edit "
      e-blog-tmp-buffer "*e-blog tmp*"
      e-blog-auth nil)

(defface e-blog-url '((t :foreground "orange")) "Face used by e-blog for url line.")
(defface e-blog-title '((t :foreground "red")) "Face used by e-blog for title line.")
(defface e-blog-label '((t :foreground "green")) "Face used by e-blog for label line.")
(defface e-blog-post '((t :foreground "purple")) "Face used by e-blog for post-separator line.")
(defface e-blog-blog '((t (:foreground "cyan1" :underline t))) "Face used by e-blog for blog titles.")

(defun e-blog-get-credentials ()
  "Gets username and password via the minibuffer."
  (setq e-blog-user (read-from-minibuffer "Username: ")
	e-blog-passwd (read-passwd "Password: ")))

(defun e-blog-fetch-auth ()
  "Calls curl to request an authorization string for further
communication with the Gdata API."
  (e-blog-get-credentials)
  (let (switch common user pass source service)
    (setq switch "-d"
	  ampersand "\&"
	  user (concat "Email=" e-blog-user)
	  pass (concat "Passwd=" e-blog-passwd)
	  source (concat "source=" e-blog-name "-" e-blog-name "-" e-blog-version)
	  service (concat "service=" e-blog-service)
	  all (concat switch user ampersand pass ampersand
		      source ampersand service))
    (call-process "curl" nil e-blog-buffer nil
		  "--stderr" "/dev/null"
		  all e-blog-fetch-authinfo-url)))

(defun e-blog-check-authinfo ()
  (condition-case nil 
      (e-blog-extract-authinfo)
    (error
     (message "No authorization token was received.
Perhaps you mistyped your username or password."))))

(defun e-blog-extract-authinfo ()
  (set-buffer e-blog-buffer)
  (let (beg)
    (search-backward "Auth=")
    (setq beg (+ (point) 5))
    (move-end-of-line 1)
    (setq e-blog-auth
	  (concat
	   "Authorization: GoogleLogin auth="
	   (buffer-substring beg (point))))
    (erase-buffer)))

(defun e-blog-fetch-bloglist ()
  (let (feed)
    (set-buffer e-blog-buffer)
    (erase-buffer)
    (message "Requesting list of blogs...")
    (call-process "curl" nil e-blog-buffer nil
		  "--stderr" "/dev/null"
		  "--header"
		  e-blog-auth
		  e-blog-fetch-bloglist-url)
    (setq feed (buffer-substring (point-min) (point-max)))
    (message "Requesting list of blogs... Done.")
    feed))

(defun e-blog-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun e-blog-forward-button ()
  (interactive)
  (forward-button 1 t))

(defun e-blog-setup-choose-buffer (feed)
  (let (url)
    (set-buffer (get-buffer-create e-blog-choose-buffer))
    (erase-buffer)
    (insert-string
     (format "%d blogs found for %s:\n\n"
	     (length (e-blog-get-titles feed)) e-blog-user))
    (dolist (title (e-blog-get-titles feed))
      (insert-string "\t")
      (insert-text-button
       "+"
       'action 'e-blog-list-posts
       'face 'e-blog-label
       'title title
       'feed feed)
      (insert " ")
      (setq url (e-blog-get-post-url title feed))
      (insert-text-button
       title
       'action 'e-blog-setup-post-buffer
       'face 'e-blog-blog
       'url url)
      (insert-string "\n"))
    (insert-string "\nSelect which blog you would like to post to.")
    (local-set-key "\t" 'e-blog-forward-button)
    (local-set-key "q" 'e-blog-kill-current-buffer)
    (goto-char (point-min))
    (switch-to-buffer e-blog-choose-buffer)))

(defun e-blog-fetch-blog-feed (url)
  (let (string)
    (save-excursion
      (set-buffer (get-buffer-create e-blog-tmp-buffer))
      (erase-buffer)
      (message "Requesting feed...")
      (call-process "curl" nil e-blog-tmp-buffer nil
		    "--stderr" "/dev/null"
		    "--header"
		    e-blog-auth
		    url)
      (setq string (buffer-substring (point-min) (point-max))))
    (message "Requesting feed... Done.")
    string))

(defun e-blog-expanded-to-collapsed ()
  (save-excursion
    (delete-char 1)
    (insert-text-button "+"
			'action 'e-blog-expand-list
			'face 'e-blog-label)))

(defun e-blog-collapsed-to-expanded ()
  (save-excursion
    (delete-char 1)
    (insert-text-button "-"
			'action 'e-blog-collapse-list
			'face 'e-blog-label)))

(defun e-blog-collapse-list (button)
  (save-excursion
    (let (beg button-pos collapsed)
      (setq button-pos (point))
      (forward-line 1)
      (setq beg (point))
      (search-forward "[X]\n\n")
      (setq collapsed (buffer-substring beg (point)))
      (delete-region beg (point))
      (goto-char button-pos)
      (delete-char 1)
      (insert-text-button "+"
			  'action 'e-blog-expand-list
			  'face 'e-blog-label
			  'collapsed collapsed))))

(defun e-blog-expand-list (button)
  (save-excursion
    (move-end-of-line 1)
    (insert "\n" (button-get button 'collapsed)))
  (e-blog-collapsed-to-expanded))

(defun e-blog-list-posts (button)
  (let (blog-title user-feed blog-feed xml current-entry)
    (setq blog-title (button-get button 'title)
	  user-feed (button-get button 'feed))
    (setq xml (e-blog-fetch-blog-feed
	       (e-blog-get-post-url blog-title user-feed)))
    (setq blog-feed (e-blog-parse-xml xml))
    (save-excursion
      (move-end-of-line 1)
      (insert "\n")
      (dolist (title (e-blog-get-titles blog-feed))
	(setq current-entry
	      (e-blog-get-entry title blog-feed))
	(insert "\t    * ")
	(insert-text-button title
			    'action 'e-blog-edit-post
			    'face 'e-blog-url
			    'entry current-entry)
	(insert " [")
	(insert-text-button "X"
			    'action 'e-blog-confirm-delete
			    'face 'e-blog-title
			    'entry current-entry)
	(insert "]")
	(insert "\n"))))
  (e-blog-collapsed-to-expanded))

(defun e-blog-get-edit-url (entry)
  (let (edit-url)
    (setq edit-url
	  (nth 1 (assoc "edit" (e-blog-get-links entry))))
    edit-url))

(defun e-blog-insert-labels (labels)
  (let (num-labels counter)
    (setq counter (length labels)
	  num-labels counter)
    (dolist (label labels)
      (setq counter (- counter 1))
      (insert label)
      (if (> counter 0)
	  (insert ", ")
	()))))

(defun e-blog-setup-post-buffer (button)
  (let (url)
    (setq url (button-get button 'url))
    (set-buffer (get-buffer-create e-blog-post-buffer))
    (e-blog-setup-common)
    (goto-char (point-min))
    (search-forward ": ")
    (insert url)
    (forward-char 1)
    (if e-blog-display-url
	()
      (narrow-to-region (point) (point-max)))
    (move-end-of-line 1)
    (local-set-key "\C-c\C-c" 'e-blog-extract-for-post)
    (switch-to-buffer e-blog-post-buffer)))
    
(defun e-blog-post (prop-list)
  (let (title content labels url rlist slist counter node-name)
    (kill-buffer (current-buffer))
    (setq title (nth 0 prop-list)
	  content (nth 1 prop-list)
	  labels (nth 2 prop-list)
	  url (nth 3 prop-list)
	  node-name "<category scheme=\"http://www.blogger.com/atom/ns#\" term=\"")
    (set-buffer (get-buffer-create e-blog-tmp-buffer))
    (erase-buffer)
    (insert e-blog-post-xml)
    (goto-char (point-min))
    (setq rlist '("<!-- @@@Title@@@ -->"
		  "<!-- @@@Text@@@ -->"
		  "<!-- @@@User Name@@@ -->"
		  "<!-- @@@email@@@ -->"))
    (setq slist (list title
		      content
		      user-full-name
		      e-blog-user))
    (setq counter 0)
    (dolist (repl rlist)
      (search-forward repl nil t)
      (replace-match (nth counter slist))
      (setq counter (+ counter 1)))
    (goto-char (point-min))
    (search-forward "</title>")
    (insert "\n")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert "  " node-name label "\"/>\n")))
    (delete-blank-lines)
    (set-visited-file-name "/tmp/e-blog-tmp")
    (save-buffer)
    (message "Sending Post...")
    (call-process "curl" nil e-blog-buffer nil
		  "-v" "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-d" "@/tmp/e-blog-tmp"
		  url)
    (e-blog-cleanup)
    (message "Sending Post... Done.")))
      
(defun e-blog-setup-edit-buffer (title labels content edit-url)
  (let (beg-narrow beg-content)
  (set-buffer (get-buffer-create
	       (concat e-blog-edit-buffer title "*")))
  (e-blog-setup-common)
  (goto-char (point-min))
  (move-end-of-line 1)
  (insert edit-url)
  (forward-line 1)
  (setq beg-narrow (point))
  (move-end-of-line 1)
  (insert title)
  (forward-line 1)
  (move-end-of-line 1)
  (e-blog-insert-labels labels)
  (goto-char (point-max))
  (insert content)
  (e-blog-do-markdowns)
  (if e-blog-display-url
      ()
    (narrow-to-region beg-narrow (point-max)))
  (local-set-key "\C-c\C-c" 'e-blog-extract-for-edit)
  (switch-to-buffer (concat e-blog-edit-buffer title "*"))))

(defun e-blog-extract-common ()
  (let (beg title content labels url post-info)
    (widen)
    (goto-char (point-min))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq url (buffer-substring beg (point)))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq title (buffer-substring beg (point)))
    (search-forward ": ")
    (setq labels (e-blog-extract-labels))
    (forward-line 2)
    (setq beg (point))
    (e-blog-do-markups)
    (setq content (buffer-substring beg (point-max)))
    (setq post-info (list title content labels url))
    post-info))

(defun e-blog-do-markups ()
  "Prepends `<p>' to the beginning of each paragraph.  Ends each
paragraph with `</p>'."
  (interactive)
  (let (replacements beg-text)
    (setq replacements
	  '(("\n\n" "</p><p>")
	    ("\n" " ")
	    ("</p><p>" "</p>\n<p>")))
    (setq beg-text (point))
    (insert-string "<p>")
    (dolist (list replacements)
      (while (search-forward (car list) nil t)
	(replace-match (nth 1 list)))
      (goto-char beg-text))
    (goto-char (point-max))
    (insert-string "</p>")))

(defun e-blog-post-edit (prop-list)
  (let (title content labels url entry)
    (kill-buffer (current-buffer))
    (setq title (nth 0 prop-list)
	  content (nth 1 prop-list)
	  labels (nth 2 prop-list)
	  url (nth 3 prop-list))
    (setq entry
	 (xml-node-name (e-blog-parse-xml
	   (e-blog-fetch-blog-feed url))))
    (e-blog-change-title entry title)
    (e-blog-change-content entry content)
    (e-blog-elisp-to-xml entry)
    ;; The rest of this function is performed in `e-blog-tmp-buffer'
    ;; since the `e-blog-elisp-to-xml' did a `set-buffer'.
    (e-blog-change-labels labels)
    (set-visited-file-name "/tmp/e-blog-tmp")
    (save-buffer)
    (message "Sending request for edit...")
    (call-process "curl" nil e-blog-buffer nil
		  "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-X" "PUT" "-d" "@/tmp/e-blog-tmp"
		  url)
    (e-blog-cleanup)
    (message "Sending request for edit... Done." )))

(defun e-blog-cleanup ()
  (delete-file "/tmp/e-blog-tmp")
  (kill-buffer e-blog-choose-buffer)
  (kill-buffer "e-blog-tmp"))

(defun e-blog-elisp-to-xml (elisp)
  (set-buffer (get-buffer-create e-blog-tmp-buffer))
  (erase-buffer)
  (xml-debug-print-internal elisp " ")
  (goto-char (point-min))
  (search-forward "<content type=\"html\">")
  (replace-match "<content type='xhtml'>")
  (insert "<div xmlns=\"http://www.w3.org/1999/xhtml\">")
  (search-forward "</content>")
  (search-backward "</content>")
  (insert "</div>")
  (goto-char (point-min))
  (while (search-forward "&" nil t)
    (replace-match "&amp;"))
  (goto-char (point-min))
  (while (search-forward "\"" nil t)
    (replace-match "'")))


(defun e-blog-extract-for-edit ()
  (interactive)
  (e-blog-post-edit (e-blog-extract-common)))

(defun e-blog-extract-for-post ()
  (interactive)
  (e-blog-post (e-blog-extract-common)))

(defun e-blog-extract-labels ()
  (let (label labels beg eol)
    (setq beg (point)
	  labels ())
    (move-end-of-line nil)
    (setq eol (point))
    (goto-char beg)
    (while (search-forward "," eol t)
      (setq label (buffer-substring beg (- (point) 1)))
      (add-to-list 'labels label)
      (setq beg (point)))
    (add-to-list 'labels (buffer-substring beg eol))
    labels))

(defun e-blog-do-markdowns ()
  (let (beg-text beg end replacements)
    (move-beginning-of-line nil)
    (setq beg-text (point))
    (setq replacements
	  '(("&lt;" "<")
	    ("&gt;" ">")
	    ("<p>" "")
	    ("</p>" "\n\n")
	    ("<div xmlns='http://www.w3.org/1999/xhtml'>" "")
	    ("</div>" "")
	    ("<br />" "")))
    (dolist (list replacements)
      (goto-char beg-text)
      (while (search-forward (car list) nil t)
	(replace-match (nth 1 list))))
    (goto-char (point-max))
    (forward-line -2)
    (move-end-of-line nil)
    (delete-region (point) (point-max))))

(defun e-blog-edit-post (button)
  (let (entry)
    (setq entry (button-get button 'entry))
    (e-blog-setup-edit-buffer
     (button-label button)
     (e-blog-get-labels entry)
     (e-blog-get-content entry)
     (e-blog-get-edit-url entry))))

(defun e-blog-setup-common ()
  (let (u-string t-string l-string p-string all faces cur-face counter)
    (setq u-string "Url: \n"
	  t-string "Title: \n"
	  l-string "Labels: \n"
	  p-string "-------- Post Follows This Line -------- \n"
	  all (list u-string t-string l-string p-string)
	  faces '(e-blog-url e-blog-title e-blog-label e-blog-post)
	  counter 0)
    (insert u-string t-string l-string p-string)
    (goto-char (point-min))
    (dolist (string all)
      (add-text-properties (point) (- (+ (point) (length string)) 2)
			   (list 'read-only "Please type your entries after the colored text."
				 'face (nth counter faces)))
      (forward-line)
      (setq counter (+ 1 counter)))
      (auto-fill-mode 1)))

(defun e-blog-new-post ()
  "Initializes e-blog."
  (interactive)
  (if e-blog-auth
      (e-blog-choose)
    (e-blog-do-auth)))

(defun e-blog-do-auth ()
  "Calls the functions necessary for communicating with Gdata."
  (e-blog-fetch-auth)
  (if (e-blog-check-authinfo)
      (e-blog-choose)))

(defun e-blog-choose ()
  (e-blog-setup-choose-buffer (e-blog-parse-xml (e-blog-fetch-bloglist))))

(defun e-blog-parse-xml (string)
  (let (parsed)
    (save-excursion
      (set-buffer (get-buffer-create e-blog-tmp-buffer))
      (erase-buffer)
      (insert string)
      (setq parsed (xml-parse-region (point-min) (point-max)))
      parsed)))

(defun e-blog-get-titles (feed)
  (let (titles)
    (setq titles ())
    (dolist (entry (e-blog-get-entries feed))
      (add-to-list 'titles
		   (e-blog-get-title entry)))
    titles))

(defun e-blog-get-entries (feed)
  (let (entries)
    (setq entries (xml-get-children (xml-node-name feed) 'entry))
    entries))

(defun e-blog-get-title (entry)
  (let (title-tag title)
    (setq title-tag
	  (xml-get-children entry 'title))
    (setq title
	  (nth 0
	       (xml-node-children
		(xml-node-name title-tag))))
    title))

(defun e-blog-get-post-url (title feed)
  (let (post-url links)
    (dolist (entry (e-blog-get-entries feed))
      (if (equal (e-blog-get-title entry) title)
	(setq post-url
	      (nth 1 (assoc e-blog-post-url-rel
			    (e-blog-get-links entry))))))
    post-url))

(defun e-blog-get-links (entry)
  (let (links type-link)
    (dolist (link (xml-get-children entry 'link))
      (setq type-link
	    (list (xml-get-attribute link 'rel)
		  (xml-get-attribute link 'href)))
      (add-to-list 'links
		   type-link))
    links))

(defun e-blog-get-entry (title feed)
  (let (matching-entry)
  (dolist (entry (e-blog-get-entries feed))
    (if (equal (e-blog-get-title entry) title)
	(setq matching-entry entry)))
  matching-entry))

(defun e-blog-get-labels (entry)
  (let (post-labels)
    (setq post-labels ())
    (dolist (label (xml-get-children entry 'category))
      (add-to-list 'post-labels
		   (xml-get-attribute label 'term)))
    post-labels))

(defun e-blog-get-content (entry)
  (let (content)
    (setq content
	  (nth 0
	       (xml-node-children
		(xml-node-name
		 (xml-get-children entry 'content)))))
    content))

(defun e-blog-change-title (entry title)
  (setcar (xml-node-children
	   (xml-node-name
	    (xml-get-children entry 'title)))
	  title))

(defun e-blog-change-content (entry content)
  (setcar (xml-node-children
	   (xml-node-name
	    (xml-get-children entry 'content)))
	  content))

(defun e-blog-change-labels (labels)
  (let (beg node-name)
    (setq node-name "<category scheme='http://www.blogger.com/atom/ns#' term='")
    (set-buffer e-blog-tmp-buffer)
    (goto-char (point-min))
    (while (search-forward node-name nil t)
      (move-beginning-of-line 1)
      (setq beg (point))
      (move-end-of-line 1)
    (delete-region beg (point))
    (delete-blank-lines))
    (goto-char (point-min))
    (search-forward "</updated>")
    (insert "\n")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert "  " node-name label "'/>\n")))
    (delete-blank-lines)))
    
(setq e-blog-post-xml 
"<entry xmlns='http://www.w3.org/2005/Atom'>
  <title type='text'><!-- @@@Title@@@ --></title>
  <content type='xhtml'>
    <div xmlns=\"http://www.w3.org/1999/xhtml\">
      <!-- @@@Text@@@ -->
    </div>
  </content>
  <author>
    <name><!-- @@@User Name@@@ --></name>
    <email><!-- @@@email@@@ --></email>
  </author>
</entry>")
