;;; e-blog.el --- a GNU Emacs interface to Blogger
;; Author: Mikey Coulson <miketcoulson@gmail.com>
;; Copyright (C)  2007 Mikey Coulson

;; This file is not part of GNU Emacs.

;; e-blog is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; e-blog is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with e-blog; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(setq e-blog-name "eblog"
      e-blog-version "0.4"
      e-blog-service "blogger"
      e-blog-get-authinfo-url "https://www.google.com/accounts/ClientLogin"
      e-blog-buffer "*e-blog*"
      e-blog-post-buffer "*e-blog post*"
      e-blog-choose-buffer "*e-blog choose*"
      e-blog-edit-buffer "*e-blog edit*"
      e-blog-tmp-buffer "*e-blog tmp*"
      e-blog-auth nil
      e-blog-all-posts-xml nil)

(defun e-blog-get-credentials ()
  "Gets username and password via the minibuffer."
  (setq e-blog-user (read-from-minibuffer "Username: ")
	e-blog-passwd (read-passwd "Password: ")))

(defun e-blog-call-curl ()
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
		  all e-blog-get-authinfo-url)))

(defun e-blog-get-authinfo ()
  "Extracts the authorization string obtained from Gdata's
ClientLogin from e-blog's logging buffer, which has the name
`e-blog-buffer'."
  (set-buffer e-blog-buffer)
  (if (search-backward "Auth=" nil t)
      ()
    (setq e-blog-error "Authinfo not found."))
  (forward-char 5)
  (let (beg)
    (setq beg (point))
    (forward-line)
    (setq e-blog-auth
	  (concat 
	   "Authorization: GoogleLogin auth="
	   (buffer-substring beg (- (point) 1))))
    (erase-buffer)))

(defun e-blog-get-bloglist ()
  "Calls curl with a request for the metafeed containing all
blogs for the user `e-blog-user'.  This function uses the
authorization string obtained with `e-blog-get-authinfo' and
stored in the variable `e-blog-auth'."
  (set-buffer e-blog-buffer)
  (call-process "curl" nil e-blog-buffer nil
		"--stderr" "/dev/null"
		"--header"
		e-blog-auth
		"http://www.blogger.com/feeds/default/blogs")
  (setq e-blog-bloglist (buffer-substring (point-min) (point-max))))

(defun e-blog-setup-post-buffer ()
  "Creates a buffer for writing a blog post."
  (e-blog-check-for-old-post e-blog-post-buffer)
  (set-buffer (get-buffer-create e-blog-post-buffer))
  (e-blog-setup-common)
  (goto-char (point-min))
  (move-end-of-line nil)
  (local-set-key "\C-c\C-c" 'e-blog-post)
  (switch-to-buffer e-blog-post-buffer))

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

(defun e-blog-post ()
  "Extracts title and text from `e-blog-post-buffer', substitutes
necessary information in `e-blog-template', and calls curl to
send a post to the url `e-blog-post-url' with the resulting xml,
which is stored in `e-blog-sent-buffer'."
  (interactive)
  (setq e-blog-sent-buffer "*e-blog sent*")
  (let (title text beg labels)
    (set-buffer e-blog-post-buffer)
    (goto-char (point-min))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq title (buffer-substring beg (point)))
    (search-forward ": ")
    (setq labels (e-blog-extract-labels))
    (forward-line 2)
    (move-beginning-of-line nil)
    (setq beg (point))
    (e-blog-do-markups)
    (setq text (buffer-substring beg (point-max)))
    (get-buffer-create e-blog-sent-buffer)
    (set-buffer e-blog-sent-buffer)
    (insert-string e-blog-post-xml)
    (goto-char (point-min))
    (search-forward "'>")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert
	 "<category scheme='http://www.blogger.com/atom/ns#' term='"
	 label
	 "'/>")))
    (search-forward "<!-- @@@Title@@@ -->")
    (replace-match title)
    (search-forward "<!-- @@@Text@@@ -->")
    (replace-match text)
    (search-forward "<!-- @@@User Name@@@ -->")
    (replace-match user-full-name)
    (search-forward "<!-- @@@email@@@ -->")
    (replace-match e-blog-user)
    (set-visited-file-name "/tmp/e-blog-tmp")
    (save-buffer)
    (call-process "curl" nil e-blog-buffer nil
		  "-v" "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-d" "@/tmp/e-blog-tmp"
		  e-blog-post-url)
    (delete-file "/tmp/e-blog-tmp")
    (kill-buffer "e-blog-tmp")
    (kill-buffer e-blog-post-buffer)))

(defun e-blog-parse-bloglist (bloglist)
  "Extracts the titles and post urls for each blog available to
`e-blog-user', and stores them in `e-blog-blogs'."
  (let (titles beg post-url)
    (setq titles ())
    (set-buffer (get-buffer-create e-blog-tmp-buffer))
    (insert-string bloglist)
    (goto-char (point-min))
    (while (search-forward "#post" nil t)
      (search-forward "href='")
      (setq beg (point))
      (search-forward "'/>")
      (backward-char 3)
      (setq post-url (buffer-substring beg (point)))
      (search-backward "<title type='text'>")
      (forward-char 19)
      (setq beg (point))
      (search-forward "</title>")
      (backward-char 8)
      (add-to-list 'titles (list (buffer-substring beg (point)) post-url))
      (search-forward "#post" nil t))
  (setq e-blog-blogs titles))
  (kill-buffer e-blog-tmp-buffer))

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

(defun e-blog-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun e-blog-setup-choose-buffer ()
  "Used when `e-blog-user' has more than one blog available for
posting.  Sets up a buffer that allows choosing which blog to
post to."
  ;; making `e-blog-all-posts-xml' nil here keeps this variable from
  ;; expanding infinitely since it is appended to when editing posts.
  (setq e-blog-all-posts-xml nil)
  (set-buffer (get-buffer-create e-blog-choose-buffer))
  (erase-buffer)
  (local-set-key "\t" 'e-blog-forward-button)
  (local-set-key "q" 'e-blog-kill-current-buffer)
  (insert-string
   (format "%d blogs found for %s:\n\n"
	   (length e-blog-blogs) e-blog-user))
  (let (beg)
    (dolist (pair e-blog-blogs)
      (insert-string "\t")
      (insert-text-button
       "+"
       'action 'e-blog-list-posts
       'face 'custom-state
       'title (car pair))
      (insert-string " ")
      (insert-text-button
       (car pair)
       'action 'e-blog-set-post-blog
       'face 'custom-link)
      (insert-string "\n"))
    (insert-string "\nSelect which blog you would like to post to.")
    (goto-char (point-min))
    (search-forward "+" nil t)
    (forward-char -1)
    (switch-to-buffer e-blog-choose-buffer)))

(defun e-blog-forward-button ()
  (interactive)
  (forward-button 1 t))

(defun e-blog-list-posts (button)
  "Creates a list containing all of the posts for a given blog."
  (setq e-blog-post-list ())
  (let (beg blog-title blog-id url posts button-pos)
    (setq posts ()
	  blog-title (button-get button 'title)
	  button-pos (point))
    (save-excursion
      (delete-char 1)
      (insert-text-button "-"
			  'action 'e-blog-collapse-post-list
			  'face 'custom-state))
    (dolist (pair e-blog-blogs)
      (if (equal blog-title (car pair))
	  (setq e-blog-post-url (nth 1 pair))))
    (set-buffer (get-buffer-create e-blog-tmp-buffer))
    (insert e-blog-post-url)
    (move-beginning-of-line nil)
    (search-forward "feeds/")
    (setq beg (point))
    (search-forward "/")
    (setq blog-id (buffer-substring beg (- (point) 1)))
    (setq url 
	  (concat "http://www.blogger.com/feeds/" blog-id "/posts/default"))
    (call-process "curl" nil e-blog-tmp-buffer nil
		  "--stderr" "/dev/null"
		  "--header"
		  e-blog-auth
		  url)
    (move-beginning-of-line 1)
    (search-forward "<content type='html'>")
    (search-backward "<title type='text'>")
    (e-blog-extract-posts)
    (kill-buffer e-blog-tmp-buffer)
    (set-buffer e-blog-choose-buffer)
    (move-end-of-line nil)
    (insert "\n")
    (e-blog-insert-posts e-blog-post-list)
    (goto-char button-pos)))

(defun e-blog-extract-posts ()
    (let (post-title text post-id post sub-posts)
      (setq sub-posts ())
      (while (search-forward "<title type='text'>" nil t)
	(setq beg (point))
	(search-forward "<")
	(setq post-title (buffer-substring beg (- (point) 1)))
	(search-forward "<content type='html'>")
	(setq beg (point))
	(search-forward "</content>")
	(setq text (buffer-substring beg (- (point) 10)))
	(search-forward "rel='self'")
	(search-forward "default/")
	(setq beg (point))
	(search-forward "'")
	(setq post-id (buffer-substring beg (- (point) 1)))
	(setq post (list post-title blog-id post-id text))
	(add-to-list 'posts post))
      ;; inserting the current value of e-blog-all-posts-xml should
      ;; make editing posts to a blog other than the one that was most
      ;; recently expanded possible.
      (save-excursion
	(goto-char (point-min))
	(if e-blog-all-posts-xml
	    (insert e-blog-all-posts-xml)
	  ()))
      (setq e-blog-all-posts-xml
	    (buffer-substring (point-min) (point-max)))
      (setq e-blog-post-list posts)))

(defun e-blog-insert-posts (post-list)
  (save-excursion
    (dolist (post post-list)
      (insert "\t    * ")
      (insert-text-button (car post)
			  'action 'e-blog-edit-post
			  'face 'dired-warning
			  'post-info post)
      (insert " [")
      (insert-text-button "X"
			  'action 'e-blog-confirm-delete
			  'face 'info-menu-star
			  'post-info post)
      (insert "]")
      (insert "\n"))))

(defun e-blog-confirm-delete (button)
  (if (y-or-n-p "Are you sure you want to delete this post? ")
      (e-blog-delete-post button)
    (message "Post not deleted.")))

(defun e-blog-delete-post (button)
  (let (beg url post-id blog-id post-info)
    (setq post-info (button-get button 'post-info))
    (setq post-id (nth 2 post-info))
    (setq blog-id (nth 1 post-info))
    (setq url (concat "http://www.blogger.com/feeds/"
		  blog-id
		  "/posts/default/"
		  post-id))
    (call-process "curl" nil e-blog-buffer nil
		  "--header" 
		  e-blog-auth
		  "-X" "DELETE"
		  url)
    (move-beginning-of-line nil)
    (setq beg (point))
    (move-end-of-line nil)
    (delete-region beg (+ (point) 1))
    (message "Post Deleted!")))
		  
(defun e-blog-edit-post (button)
  (let (beg end post-info blog-id post-id
	text tmp-buffer post-xml title
	text label labels)
    (setq post-info (button-get button 'post-info)
	  title (nth 0 post-info)
	  post-id (nth 2 post-info)
	  text (nth 3 post-info)
	  labels ())
    (set-buffer (get-buffer-create e-blog-tmp-buffer))
    (insert e-blog-all-posts-xml)
    (goto-char (point-min))
    (search-forward (concat ".post-" post-id))
    (search-backward "<entry>")
    (setq beg (point))
    (search-forward "</entry>")
    (setq post-xml (buffer-substring beg (point)))
    (erase-buffer)
    (insert post-xml)
    (goto-char (point-max))
    (search-backward "<title type='text'>")
    (setq beg (point))
    (search-forward "</content>")
    (delete-region beg (point))
    (insert "<!-- @@@Title & Content@@@ -->")
    (goto-char (point-min))
    (while (search-forward "term='" nil t)
      (setq beg (point))
      (search-forward "'")
      (setq label (buffer-substring beg (- (point) 1)))
      (message label)
      (add-to-list 'labels label))
    (e-blog-setup-edit-buffer title text labels)))

(defun e-blog-do-markdowns ()
  (let (beg-text beg end replacements)
    (move-beginning-of-line nil)
    (setq beg-text (point)
	  beg (point))
    (search-forward "&gt;")
    (setq end (point))
    (delete-region beg end)
    (setq beg (point))
    (re-search-forward " *")
    (delete-region beg (point))
    (setq replacements
	  '(("&lt;" "<")
	    ("&gt;" ">")
	    ("<p>" "")
	    ("</p>" "\n\n")
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

(defun e-blog-post-edit ()
  (interactive)
  (let (title text beg labels)
    (set-buffer e-blog-edit-buffer)
    (goto-char (point-min))
    (search-forward ": ")
    (setq beg (point))
    (forward-line)
    (setq title (buffer-substring beg (point)))
    (search-forward ": ")
    (setq labels (e-blog-extract-labels))
    (forward-line 2)
    (setq beg (point))
    (e-blog-do-markups)
    (setq text (buffer-substring beg (point-max)))
    (e-blog-edit-formulate-xml title text labels)
    (call-process "curl" nil e-blog-buffer nil
		  "--header"
		  e-blog-auth
		  "--header" "Content-Type: application/atom+xml"
		  "-X" "PUT" "-d" "@/tmp/e-blog-tmp"
		  e-blog-edit-url)
    (delete-file "/tmp/e-blog-tmp")
    (kill-buffer e-blog-edit-buffer)
    (kill-buffer e-blog-choose-buffer)
    (kill-buffer "e-blog-tmp")
    (message "Sending request for edit... Done." )))

(defun e-blog-edit-formulate-xml (title text labels)
    (set-buffer e-blog-tmp-buffer)
    (goto-char (point-min))
    (let (beg)
      (while (search-forward
	      "<category scheme='http://www.blogger.com/atom/ns#' term='"
	      nil t)
	(replace-match "")
	(setq beg (point))
	(search-forward "'/>")
	(delete-region beg (point))))
    (goto-char (point-min))
    (search-forward "</updated>")
    (if (equal (nth 0 labels) "")
	()
      (dolist (label labels)
	(insert
	 "<category scheme='http://www.blogger.com/atom/ns#' term='"
	 label
	 "'/>")))
    (search-forward "<!-- @@@Title & Content@@@ -->")
    (replace-match
     (concat
      "<title type='text'>" title "</title>"
      "<content type='xhtml'><div xmlns=\"http://www.w3.org/1999/xhtml\">"
      text
      "</div></content>"))
    (goto-char (point-max))
    (let (beg)
      (search-backward "link rel='edit'")
      (search-forward "href='")
      (setq beg (point))
      (search-forward "'")
      (setq e-blog-edit-url (buffer-substring beg (- (point) 1))))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (search-forward "entry")
    (insert " xmlns='http://www.w3.org/2005/Atom'")
    (set-visited-file-name "/tmp/e-blog-tmp")
    (save-buffer)
    (message "Sending request for edit..."))

(defun e-blog-setup-edit-buffer (title text labels)
  (e-blog-check-for-old-post e-blog-edit-buffer)
  (set-buffer (get-buffer-create e-blog-edit-buffer))
  (e-blog-setup-common)
  (goto-char (point-min))
  (move-end-of-line nil)
  (insert title)
  (forward-line)
  (move-end-of-line nil)
  (let (num-labels counter)
    (setq counter (length labels)
	  num-labels counter)
    (dolist (label labels)
      (setq counter (- counter 1))
      (insert label)
      (if (> counter 0)
	  (insert ", ")
	())))
  (goto-char (point-max))
  (insert text)
  (local-set-key "\C-c\C-c" 'e-blog-post-edit)
  (e-blog-do-markdowns)
  (switch-to-buffer e-blog-edit-buffer))

(defun e-blog-collapse-post-list (button)
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
			'action 'e-blog-insert-collapsed
			'face 'custom-state
			'collapsed collapsed)
    (forward-char -1)))

(defun e-blog-insert-collapsed (button)
  (save-excursion
    (let (collapsed button-pos)
      (setq button-pos (point))
      (setq collapsed (button-get button 'collapsed))
      (forward-line 1)
      (insert collapsed)
      (goto-char button-pos)
      (delete-char 1)
      (insert-text-button "-"
			  'action 'e-blog-collapse-post-list
			  'face 'custom-state))))

(defun e-blog-setup-common ()
  (let (t-string l-string p-string all faces cur-face counter)
    (setq t-string "Title: \n"
	  l-string "Labels (separated by commas): \n"
	  p-string "-------- Post Follows This Line -------- \n"
	  all (list t-string l-string p-string)
	  faces '(info-menu-star custom-state info-xref-visited)
	  counter 0)
    (insert t-string l-string p-string)
    (goto-char (point-min))
    (dolist (string all)
      (add-text-properties (point) (- (+ (point) (length string)) 2)
			   (list 'read-only "Please type your entries after the colored text."
				 'face (nth counter faces)))
      (forward-line)
      (setq counter (+ 1 counter)))
      (auto-fill-mode 1)))

(defun e-blog-set-kill-buffer (name)
  (set-buffer name)
  (kill-buffer (current-buffer)))

(defun e-blog-check-for-old-post (name)
  (condition-case nil
      (e-blog-set-kill-buffer name)
    (error 
     nil)))

(defun e-blog-set-post-blog (button)
  "The callback for the buttons created in
`e-blog-setup-choose-buffer'.  Finds the post url associated with
a blog title in the list contained in `e-blog-blogs' and sets
`e-blog-post-url' accordingly."
  (message "Will post this article to `%s'."
	   (button-label button))
  (dolist (pair e-blog-blogs)
    (if (equal (button-label button) (car pair))
	(setq e-blog-post-url (nth 1 pair))))
  (e-blog-setup-post-buffer)
  (kill-buffer e-blog-choose-buffer))

(defun e-blog-do-auth ()
  "Calls the functions necessary for communicating with Gdata."
  (e-blog-call-curl)
  (e-blog-get-authinfo)
  (e-blog-get-bloglist)
  (e-blog-parse-bloglist e-blog-bloglist))

(defun e-blog-new-post ()
  "Initializes e-blog."
  (interactive)
  (if e-blog-auth
      ()
    (e-blog-do-auth))
  (e-blog-setup-choose-buffer))

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
