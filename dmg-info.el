;;; dmg-info.el --- Functions concerning data & info for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025  Daniel Griswold

;; Author: Daniel Griswold <kc5gmr@gmail.com>
;; URL: 
;; Version: 0.1.0


;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These functions have to with various kinds of information and data

;;; Code:

(defun file-updated-today-p (file)
  (string= (format-time-string "%F")
           (format-time-string "%F" (nth 6 (file-attributes file)))))

(defun dmg-get-gcal ()
  "Downloads my Google calendars then imports them into org-mode format"
  (if (file-exists-p "~/Temp/basic.ics")
      (delete-file "~/Temp/basic.ics"))
  (if (file-exists-p "~/Dropbox/Management/cal.org")
      (delete-file "~/Dropbox/Management/cal.org"))

  (shell-command "timeout 10 wget -P ~/Temp/ https://calendar.google.com/calendar/ical/kc5gmr%40gmail.com/private-57f14c7084adbb53cd4d24f3861e1a56/basic.ics" nil)
  (kill-buffer "*Shell Command Output*")
  (shell-command "ical2org.awk < ~/Temp/basic.ics > ~/Dropbox/Management/cal.org" nil)
  (delete-file "~/Temp/basic.ics")

  (shell-command "timeout 10 wget -P ~/Temp/ https://calendar.google.com/calendar/ical/u6oqtkld28hrjurq95lmipaqnc%40group.calendar.google.com/private-a802886a63f97c6fd3c8ed3cc22ad502/basic.ics" nil)
  (kill-buffer "*Shell Command Output*")
  (shell-command "ical2org.awk < ~/Temp/basic.ics >> ~/Dropbox/Management/cal.org" nil))

;;;###autoload
(defun ac (code)
  "Lookup area code"
  (interactive "sEnter area code: ")
  (find-file "/usr/share/miscfiles/na.phone")
  (occur code)
  (kill-buffer "na.phone"))

(defun dmg-weather ()
  (let* ((command "ansiweather ")
         (options "-u imperial -l 'Holland, US' -a false -s true -w true -h true -p false -i false")
         (cstring (concat command options))
         (currtemp (string-trim (shell-command-to-string cstring)))
         (wstring (replace-regexp-in-string " °F" "°" (replace-regexp-in-string "Weather in Holland: " "" currtemp)))
	 (weather (replace-regexp-in-string " - " "\n- " wstring))
         (now (format-time-string "%-k:%M")))
    (insert (concat now "\n- " weather))))

;; (defun dmg-weather2 ()
;;   (let* ((command "ansiweather ")
;;          (options "-u imperial -l 'Holland, US' -a false -s false -w true -h true -p false -i false")
;;          (cstring (concat command options))
;;          (currtemp (string-trim (shell-command-to-string cstring)))
;;          (fstring (replace-regexp-in-string " °F" "°" (replace-regexp-in-string "Weather in Holland: " "" currtemp)))
;;          (now (format-time-string "%-k:%M, ")))
;;     (concat now fstring)))

(defun dmg-weather-daily ()
  (org-beginning-of-line)
  (org-kill-line)
  (insert "Current time and weather")
  (next-line)
  (org-end-of-line)
  (dmg-weather))

(provide 'dmg-info)
