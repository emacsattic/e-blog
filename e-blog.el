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

;;; TODO: update commentary before 0.2 release.
;;; Commentary:

;; e-blog allows you to post to a single blog on Blogger.  There is not
;; yet support for selecting which blog to post to, if you have several.
;; It will post to the last blog in the metafeed returned by the blogger
;; service.  You must have curl installed and in your PATH to use e-blog

;; To use e-blog:
;;   change e-blog-template to reflect the path to e-blog-template.xml, below
;;   (load-file "path-to-e-blog/e-blog.el")
;;   call e-blog-new-post
;;   C-c C-c from within the post buffer will submit your post

(setq e-blog-name "mblog"
      e-blog-version "0.1"
      e-blog-service "blogger"
      e-blog-get-authinfo-url "https://www.google.com/accounts/ClientLogin"
      e-blog-buffer "*e-blog*"
      e-blog-auth nil)

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
  "Extracts the authorization string obtained from Gdata's ClientLogin from e-blog's logging buffer, which has the name `e-blog-buffer'."
  (set-buffer e-blog-buffer)
  (if (search-backward "Auth=" nil t)
      ()
    (setq e-blog-error "Authinfo not found."))
  (forward-char 5)
  (let (start)
    (setq start (point))
    (forward-line)
    (setq e-blog-auth
	  (buffer-substring start (- (point) 1)))))

(defun e-blog-get-bloglist ()
  "Calls curl with a request for the metafeed containing all
blogs for the user `e-blog-user'.  This function uses the
authorization string obtained with `e-blog-get-authinfo' and
stored in the variable `e-blog-auth'."
  (set-buffer e-blog-buffer)
  (let (start end address authheader)
    (setq start (point))
    (setq authheader
	  (concat "Authorization: GoogleLogin auth=" e-blog-auth))
    (call-process "curl" nil e-blog-buffer nil
		  "--stderr" "/dev/null"
		   "--header"
		   authheader
		  "http://www.blogger.com/feeds/default/blogs")
    (setq end (goto-char (point-max)))
    (setq e-blog-bloglist (buffer-substring start end))))

(defun e-blog-get-post-url ()
  "Used when there is only one blog available for posting for
`e-blog-user'.  Extracts the post url for this single blog and
stores it in `e-blog-post-url'."
  (let (start end)
    (set-buffer e-blog-buffer)
    (search-backward "#post")
    (search-forward "href='")
    (setq start (point))
    (search-forward "'")
    (setq end (- (point) 1))
    (goto-char (point-max))
    (setq e-blog-post-url (buffer-substring start end))))

(defun e-blog-setup-post-buffer ()
  "Creates a buffer for writing a blog post."
  (setq e-blog-post-buffer "*e-blog post*")
  (get-buffer-create e-blog-post-buffer)
  (set-buffer e-blog-post-buffer)
  (let (pos)
    (insert "Title: \n")
    (setq pos (- (point) 1))
    (insert "-------- Post Follows This Line --------\n")
    (goto-char pos))
  (add-text-properties 1 7
		       '(read-only "Please the title after the colon."
			 face info-menu-star))
  (add-text-properties 9 49
		       '(read-only "Please type your post below this line."
			 face info-xref-visited))
  (local-set-key "\C-c\C-c" 'e-blog-post)
  (switch-to-buffer e-blog-post-buffer))

(defun e-blog-do-markups ()
  "Prepends `<p>' to the beginning of each paragraph.  Ends each
paragraph with `</p>'."
  (interactive)
  (insert-string "<p>")
  (while (search-forward "\n\n" nil t)
    (replace-match "</p>\n<p>"))
  (goto-char (point-max))
  (insert-string "</p>\n"))

(defun e-blog-post ()
  "Extracts title and text from `e-blog-post-buffer', substitutes
necessary information in `e-blog-template', and calls curl to
send a post to the url `e-blog-post-url' with the resulting xml,
which is stored in `e-blog-sent-buffer'."
  (interactive)
  (setq e-blog-sent-buffer "*e-blog sent*")
  (let (title text)
    (set-buffer e-blog-post-buffer)
    (goto-char (point-min))
    (search-forward ":")
    (let (start end)
      (forward-char 1)
      (setq start (point))
      (forward-line)
      (setq end (point))
      (setq title (buffer-substring start end)))

    (let (start)
      (forward-line)
      (setq start (point))
      (e-blog-do-markups)
      (setq text (buffer-substring start (point-max))))

    (get-buffer-create e-blog-sent-buffer)
    (set-buffer e-blog-sent-buffer)
    (insert-string e-blog-post-xml)
    (goto-char (point-min))
    (search-forward "<!-- @@@Title@@@ -->")
    (replace-match title)
    (search-forward "<!-- @@@Text@@@ -->")
    (replace-match text)
    (search-forward "<!-- @@@User Name@@@ -->")
    (replace-match user-full-name)
    (search-forward "<!-- @@@email@@@ -->")
    (replace-match e-blog-user))
    (set-visited-file-name "/tmp/e-blog-tmp")
    (save-buffer)
    (call-process "curl" nil e-blog-buffer nil
		  "-v" "--header"
		  (concat "Authorization: GoogleLogin auth="
			  e-blog-auth)
		  "--header" "Content-Type: application/atom+xml"
		  "-d" "@/tmp/e-blog-tmp"
		  e-blog-post-url)
  (delete-file "/tmp/e-blog-tmp")
  (kill-buffer "e-blog-tmp")
  (kill-buffer e-blog-post-buffer))

(defun e-blog-parse-bloglist (bloglist)
  "Extracts the titles and post urls for each blog available to
`e-blog-user', and stores them in `e-blog-blogs'."
  (let (tmp-buffer titles beg end)
    (setq tmp-buffer "*e-blog tmp bloglist*")
    (setq titles ())
    (get-buffer-create tmp-buffer)
    (set-buffer tmp-buffer)
    (insert-string bloglist)
    (goto-char (point-min))
    (let (post-url beg-url end-url)
      (while (search-forward "#post" nil t)
	(search-forward "href='")
	(setq beg-url (point))
	(search-forward "'/>")
	(backward-char 3)
	(setq end-url (point))
	(setq post-url (buffer-substring beg-url end-url))
	(search-backward "<title type='text'>")
	(forward-char 19)
	(setq beg (point))
	(search-forward "</title>")
	(backward-char 8)
	(setq end (point))
	(add-to-list 'titles (list (buffer-substring beg end) post-url))
	(search-forward "#post" nil t)))
    (setq e-blog-blogs titles)
    (kill-buffer tmp-buffer)))

(defun e-blog-setup-choose-buffer ()
  "Used when `e-blog-user' has more than one blog available for
posting.  Sets up a buffer that allows choosing which blog to
post to."
  (let (choose-buffer)
    (setq choose-buffer "*e-blog choose*")
    (get-buffer-create choose-buffer)
    (set-buffer choose-buffer)
    (insert-string
     (format "%d blogs found for %s:\n\n"
	     (length e-blog-blogs) e-blog-user))
    (let (beg)
      (dolist (pair e-blog-blogs)
	(insert-string "\t")
	(insert-text-button
	 "+"
	 'action 'e-blog-list-posts)
	(insert-string " ")
	(insert-text-button
	 (car pair)
	 'action 'e-blog-set-post-blog)
	(insert-string "\n"))
      (insert-string "\nSelect which blog you would like to post to.")
      (goto-char (point-min))
      (search-forward "+ " nil t)
      (switch-to-buffer choose-buffer)
      (setq e-blog-choose-buffer choose-buffer))))

(defun e-blog-list-posts (button)
  "Creates a list containing all of the posts for a given blog."
  (message "Sorry, listing posts is not yet implemented."))

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

(defun e-blog-single-blog ()
  "Sets the user up for posting only to a single blog."
  (setq e-blog-post-url (car (car e-blog-blogs)))
  (e-blog-setup-post-buffer))

(defun e-blog-multi-blog ()
  "Sets the user up for posting to multiple blogs."
  (e-blog-setup-choose-buffer))

(defun e-blog-check-multi ()
  "Checks whether the user has multiple blogs available for
posting."
  (if (> (length e-blog-blogs) 1)
      (e-blog-multi-blog)
    (e-blog-single-blog)))

(defun e-blog-new-post ()
  "Initializes e-blog."
  (interactive)
  (if e-blog-auth
      ()
    (e-blog-do-auth))
  (e-blog-check-multi))

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
