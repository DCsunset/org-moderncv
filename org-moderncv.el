;;; org-moderncv.el --- LaTeX moderncv Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Author: DCsunset, Oscar Najera <hi AT oscarnajera.com DOT com>
;; Version: 0.1.1
;; Keywords: org, tex
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023 DCSunset
;; Copyright (C) 2018 Oscar Najera

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a LaTeX moderncv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'cl-lib)
(require 'ox-latex)
(require 'org-cv-utils)

;; Install a default set-up for moderncv export.
(unless (assoc "moderncv" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("moderncv"
                 "\\documentclass{moderncv}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options specific for using the moderncv class in LaTeX export."
  :tag "Org moderncv"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'moderncv 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "moderncv" t)
    ;; can be "cv" or "letter" to make the title
    (:cvkind "CVKIND" nil "cv" t)
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvcolor "CVCOLOR" nil nil t)
    ;; whether to disable italic font in all subtitles
    (:noitalic "NOITALIC" nil nil t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:social "SOCIAL" nil nil newline)
    ;; cover letter
    (:recipient "RECIPIENT" nil nil newline)
    (:opening "OPENING" nil nil parse)
    (:closing "CLOSING" nil nil parse))
  ; append to LaTeX menu entry
  :menu-entry
  '(?l 1
       ((?c "As CV PDF file" org-moderncv-export-to-pdf)
        (?C "As CV LaTeX file" org-moderncv-export-to-latex)))
  :translate-alist '((template . org-moderncv-template)
                     (headline . org-moderncv-headline)))


;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and moderncv themes.

(defun org-moderncv-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; Necessary packages used later
     "\\usepackage{xpatch}\n"
     ;; cvstyle
     (let ((cvstyle (org-export-data (plist-get info :cvstyle) info)))
       (when cvstyle (format "\\moderncvstyle{%s}\n" cvstyle)))
     ;; cvcolor
     (let ((cvcolor (org-export-data (plist-get info :cvcolor) info)))
       (when (not (string-empty-p cvcolor)) (format "\\moderncvcolor{%s}\n" cvcolor)))
     ;; disable itatic font for subtitles
     (let ((noitalic (org-export-data (plist-get info :noitalic) info)))
       (when (not (string-empty-p noitalic))
         ;; itshape and slshape are used in different styles
         ;; so remove both
         (concat "\\xpatchcmd{\\cventry}{\\slshape}{}{}{}\n"
                 "\\xpatchcmd{\\cventry}{\\itshape}{}{}{}\n")))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
                        (let ((auth (plist-get info :author)))
                          (and auth (org-export-data auth info))))))
       (format "\\name{%s}{}\n" author))
     ;; photo
     (let ((photo (org-export-data (plist-get info :photo) info)))
       (when (org-string-nw-p photo)
         (format "\\photo{%s}\n" photo)))
     ;; email
     (let ((email (org-export-data (plist-get info :email) info)))
       (when (org-string-nw-p email)
         (format "\\email{%s}\n" email)))
     ;; phone
     (let ((mobile (org-export-data (plist-get info :mobile) info)))
       (when (org-string-nw-p mobile)
         (format "\\phone[mobile]{%s}\n" mobile)))
     ;; homepage
     (let ((homepage (org-export-data (plist-get info :homepage) info)))
       (when (org-string-nw-p homepage)
         (format "\\homepage{%s}\n" homepage)))
     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when (org-string-nw-p address)
         (format "\\address%s\n" (mapconcat (lambda (line)
                                              (format "{%s}" line))
                                            (split-string address "\n") ""))))

     ;;; For cover letter
     ;; recipient
     (let ((recipient (org-export-data (plist-get info :recipient) info)))
       (when (org-string-nw-p recipient)
         (format "\\recipient%s\n" (mapconcat (lambda (line)
                                              (format "{%s}" line))
                                            (split-string recipient "\n") ""))))
     ;; opening
     (let ((opening (org-export-data (plist-get info :opening) info)))
       (when (org-string-nw-p opening)
         (format "\\opening{%s}\n" opening)))
     ;; closing
     (let ((closing (org-export-data (plist-get info :closing) info)))
       (when (org-string-nw-p closing)
         (format "\\closing{%s}\n" closing)))

     (mapconcat (lambda (social-network)
                  (let ((network (org-export-data
                                  (plist-get info (car social-network)) info)))
                    (when (org-string-nw-p network)
                      (format "\\social[%s]{%s}\n"
                              (nth 1 social-network) network))))
                '((:github "github")
                  (:gitlab "gitlab")
                  (:linkedin "linkedin"))
                "")

     ;; each social line is in format: kind | content [| url]
     (let ((social (org-export-data (plist-get info :social) info)))
       (when (org-string-nw-p social)
         (mapconcat (lambda (line)
                      (let* ((items (mapcar #'string-trim (split-string line "|")))
                             (nargs (length items)))
                        (when (or (> nargs 3)
                                  (< nargs 2))
                          (error "Invalid number of args in social"))
                        (let ((kind (nth 0 items))
                              (content (nth 1 items))
                              (url (or (nth 2 items) "")))
                          (format "\\social[%s][%s]{%s}\n" kind url content))))
                    (split-string social "\n") "")))

     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))

     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
            (formatted-subtitle
             (when subtitle
               (format (plist-get info :latex-subtitle-format)
                       (org-export-data subtitle info))))
            (separate (plist-get info :latex-subtitle-separate)))
       (concat
        (format "\\title{%s%s}\n" title
                (if separate "" (or formatted-subtitle "")))
        (when (and separate subtitle)
          (concat formatted-subtitle "\n"))))

     ;; patch makesletterclosing cmd to make spacing better
     "\\patchcmd{\\makeletterclosing}{[3em]}{[0.5em]}{}{}\n"
     "\\makeatletter
      \\patchcmd{\\makeletterhead}
        {\\ifthenelse{\\isundefined{\\@extrainfo}}{}{\\makenewline\\@extrainfo}}
        {
          \\def\\socialsdetails{}%
          \\collectionloop{socials}{% the key holds the social type (=symbol command prefix), the item holds the link
            \\protected@edef\\socialsdetails{\\socialsdetails\\protect\\makenewline\\csname\\collectionloopkey socialsymbol\\endcsname\\collectionloopitem}}%
          \\socialsdetails%
          \\ifthenelse{\\isundefined{\\@extrainfo}}{}{\\makenewline\\@extrainfo}%
        }
        {}{}
      \\makeatother\n"

     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let ((cvkind (org-export-data (plist-get info :cvkind) info)))
       (when cvkind (format "\\make%stitle\n" cvkind)))
     ;; Set up hyperref here because it's only included in moderncv
     ;; Document's body.
     "\n\n"
     contents
     "\n\n"
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     (let ((cvkind (org-export-data (plist-get info :cvkind) info)))
       ;; increase spacing between content and closing
       (when (equal cvkind "letter") "\\vspace*{2em}\\makeletterclosing\n"))
     "\\end{document}")))


(defun org-moderncv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let ((cvstyle (org-export-data (plist-get info :cvstyle) info))
        (entry (org-cv-utils--parse-cventry headline info))
        (note (or (org-element-property :NOTE headline) "")))
    (format "\\cventry{%s}{%s}{%s}{%s}{%s}{%s}\n"
            (org-cv-utils--format-time-window (alist-get 'from-date entry)
                                              (alist-get 'to-date entry))
            ; title is the first line in any style
            (alist-get (if (equal cvstyle "banking") 'subtitle 'title) entry)
            (alist-get (if (equal cvstyle "banking") 'title 'subtitle) entry)
            (alist-get 'location entry)
            note contents)))

;;;; Headline
(defun org-moderncv-headline (headline contents info)
  "Transcode HEADLINE element into moderncv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CVENV headline)))
                         (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-moderncv--format-cventry headline contents info))
       ((org-export-with-backend 'latex headline contents info))))))


;;;###autoload
(defun org-moderncv-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX file.

Based on `org-latex-export-to-latex'.
Disable hyperref package to prevent duplicate import.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
        ; disable hyperref as moderncv already includes it
        (org-latex-default-packages-alist
         (seq-remove (lambda (e) (equal (nth 1 e) "hyperref"))
                     org-latex-default-packages-alist)))
    (org-export-to-file 'moderncv outfile
      async subtreep visible-only body-only ext-plist)))


;;;###autoload
(defun org-moderncv-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

Based on `org-latex-export-to-pdf'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
        ; disable hyperref as moderncv already includes it
        (org-latex-default-packages-alist
         (seq-remove (lambda (e) (equal (nth 1 e) "hyperref"))
                     org-latex-default-packages-alist)))
    (org-export-to-file 'moderncv outfile
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))


(provide 'org-moderncv)
;;; org-moderncv ends here
