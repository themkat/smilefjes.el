;;; smilefjes.el --- Check if a restaurant has a good rating with Mattilsynet (food administration in Norway) -*- lexical-binding: t; -*-

;; URL: https://github.com/themkat/mos-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (request "0.3.2") (ht "2.3") (dash "2.19.1") (helm "3.8.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple package for checking if a restaurant have a good rating with the
;; food administration in Norway (Mattilsynet). The rating is given as a smiley
;; (smile is obviously good, straight mouth meh, sad is not so good).
;; Determined by hygienic stuff and more, and is not a test of tastiness. 
;; Emojify mode is recommended, but not required.

;;; Code:
(require 'request)
(require 'json)
(require 'helm)
(require 'dash)
(require 'ht)

(defun smilefjes--cities-to-helm-sources (cities)
  (-map (lambda (city-info)
          (-let [(&hash "name" name) city-info]
            name))
        (ht-get cities "codes")))

(defvar smilefjes-selected-city nil)
(defun smilefjes-select-city (cities)
  (let ((helm-cities (smilefjes--cities-to-helm-sources cities)))
    (helm :sources (helm-build-sync-source "cities"
                     :candidates helm-cities
                     :action (lambda (city)
                               (setq smilefjes-selected-city city)))
          :buffer "*smilefjes-select-city*")))

(defun smilefjes-fetch-cities ()
  (request "https://data.ssb.no/api/klass/v1/classifications/131/codes?from=2022-06-06"
    :headers '(("accept" . "application/json"))
    :parser (lambda ()
              (let ((json-object-type 'hash-table)
                    (json-array-type 'list))
                (json-read)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (smilefjes-select-city data)))
    ;; not recommended, but makes our code easier to handle and reason about :P 
    :sync t
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Failed to fetch Norwegian cities!")))))


(defun smilefjes--restaurants-to-helm-sources (cities)
  (-map (lambda (city-info)
          (-let [(&hash "navn" name "total_karakter" rating) city-info]
            (cons name rating)))
        (ht-get cities "entries")))

(defvar smilefjes-selected-restaurant nil)
(defun smilefjes-select-restaurant (restaurants)
  (let ((helm-restaurants (smilefjes--restaurants-to-helm-sources restaurants)))
    (helm :sources (helm-build-sync-source "restaurants"
                     :candidates (mapcar 'car helm-restaurants)
                     :action (lambda (restaurant)
                               (setq smilefjes-selected-restaurant (assoc restaurant helm-restaurants))))
          :buffer "*smilefjes-select-restaurant*")))

(defvar smilefjes-restaurants-complete nil)
(defun smilefjes-fetch-mattilsynet-reports (city &optional page)
  (request (concat "https://hotell.difi.no/api/json/mattilsynet/smilefjes/tilsyn?poststed=" city
                   "&page=" (number-to-string (or page 1)))
      :headers '(("accept" . "application/json"))
      :parser (lambda ()
                (let ((json-object-type 'hash-table)
                      (json-array-type 'list))
                  (json-read)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((total-pages (ht-get data "pages"))
                        (current-page (or page 1)))
                    (if (< current-page total-pages)
                        (progn
                          (ht-set! smilefjes-restaurants-complete "entries" (append (ht-get data "entries")
                                                                                    (ht-get smilefjes-restaurants-complete "entries")))
                          (smilefjes-fetch-mattilsynet-reports city (+ current-page 1)))
                      (smilefjes-select-restaurant smilefjes-restaurants-complete)))))
      ;; not recommended, but makes our code easier to handle and reason about :P 
      :sync t
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Failed to fetch Mattilsynet reports!")))))

(defun smilefjes-rating-to-emoji (rating)
  (cond ((or (string-equal rating "0")
             (string-equal rating "1"))
         ":)")
        ((string-equal rating "2")
         ":/")
        ((string-equal rating "3")
         ":(")
        (t
         "Not rated")))

(defface smilefjes-face
  '((t :height 2.5))
  "Face to make the size a little bigger in the result buffers.")

(defun smilefjes ()
  "Main entrypoint."
  (interactive)
  ;; hack to allow bigger data sets to run my awful recursive algorithm above
  (let ((max-lisp-eval-depth 10000))
    (setq smilefjes-restaurants-complete (ht ("entries" '())))
    (smilefjes-fetch-cities)
    (smilefjes-fetch-mattilsynet-reports smilefjes-selected-city))
  
  ;; Create a buffer with a reports
  (let* ((restaurant-name (car smilefjes-selected-restaurant))
         (restaurant-rating (cdr smilefjes-selected-restaurant))
         (report-buffer (generate-new-buffer (concat "*Mattilsynet smilefjes report for " restaurant-name "*"))))
    (with-current-buffer report-buffer
      (insert restaurant-name)
      (insert ?\n)
      (insert "Rating: ")
      (insert (smilefjes-rating-to-emoji restaurant-rating))
      (when (require 'emojify nil t)
        (emojify-mode))
      (buffer-face-set 'smilefjes-face)
      (read-only-mode))
    (switch-to-buffer report-buffer)))

(provide 'smilefjes)
;;; smilefjes.el ends here
