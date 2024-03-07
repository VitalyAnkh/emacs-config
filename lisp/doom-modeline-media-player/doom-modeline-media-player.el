;;; doom-modeline-media-player.el --- Segmant for media/playback information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 05, 2024
;; Modified: March 05, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/tecosaur/doom-modeline-media-player
;; Package-Requires: ((emacs "25.1") (doom-modeline "4.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Segmant for media/playback information
;;
;;; Code:

(require 'doom-modeline)
(require 'mpris)

(declare-function mpris-track-attr "mpris")

(defgroup doom-modeline-media-player nil
  "Media Player information via the dBus API."
  :group 'doom-modeline
  :prefix "doom-modeline-media-player-")

(defcustom doom-modeline-media-player t
  "Whether to display the media player.
Can be a boolean value, or a predicate function."
  :type '(choice bool function))

(defcustom doom-modeline-media-player-playback-indication 'button
  "Whether playback status (playing/paused) should be shown in the modeline."
  :type '(choice (const button) (const dim) (const nil)))

(defcustom doom-modeline-media-player-show-albumart t
  "Whether album art should be shown in the modeline."
  :type 'bool)

(defcustom doom-modeline-media-player-albumart-height 1.4
  "The height of the album art image, in em."
  :type 'number)

(defcustom doom-modeline-media-player-display-text "%t"
  "The format of the text shown in the modeline segment.
The format spec can include the following placeholders:
- %t for the track title
- %a for the track artist
- %A for the album artist
- %r for the album name
- %g for the genre
- %r for the rating
- %l for the length of the track"
  :type 'string)

(defcustom doom-modeline-media-player-display-max-width 30
  ""
  :type 'integer)

(defcustom doom-modeline-media-player-display-hover-text "%t by %a (%l)"
  "The format of the text shown when hovering over the modeline segment.
The format spec can include the following placeholders:
- %t for the track title
- %a for the track artist
- %A for the album artist
- %r for the album name
- %g for the genre
- %r for the rating
- %l for the length of the track"
  :type 'string)

(defcustom doom-modeline-media-player-art-hover-text "%r by %A"
  "The format of the text shown when hovering over the album art.
The format spec can include the following placeholders:
- %t for the track title
- %a for the track artist
- %A for the album artist
- %r for the album name
- %g for the genre
- %r for the rating
- %l for the length of the track"
  :type 'string)

(defface doom-modeline-media-player-text
  '((t (:inherit doom-modeline :weight 'medium)))
  "Face for media information text in the modeline.")

(defface doom-modeline-media-player-text-dim
  '((t (:inherit (doom-modeline-media-player-text shadow))))
  "Face for modeline text when playback is paused/stopped.
Used when `doom-modeline-media-player-playback-indication' is set to dim.")

(defvar doom-modeline-media-player--is-setup nil)

(defun doom-modeline-media-player--setup ()
  "Setup the modeline."
  (add-hook 'mpris-current-status-hook
            #'doom-modeline-media-player--react-to-change)
  (setq doom-modeline-media-player--is-setup t))

(defun doom-modeline-media-player--teardown ()
  "Teardown the modeline."
  (remove-hook 'mpris-current-status-hook
               #'doom-modeline-media-player--react-to-change)
  (setq doom-modeline-media-player--is-setup nil))

(defvar doom-modeline-media-player--last-draw-time 0.0)

(defun doom-modeline-media-player--react-to-change (nature _details)
  "Handle a change event (NATURE _DETAILS) from MPRIS."
  (if (< (- (float-time) doom-modeline-media-player--last-draw-time)
         display-time-interval)
      (when (or (and (eq nature 'playback)
                     doom-modeline-media-player-playback-indication)
                (eq nature 'metadata))
        (force-mode-line-update t))
    (doom-modeline-media-player--teardown)))

(defun doom-modeline-media-player--format-length (us)
  "Format a duriation US (microseconds) into a MmSSs format."
  (let* ((seconds (/ us 1000000))
         (minutes (/ seconds 60)))
    (format "%dm%02ds" minutes (% seconds 60))))

(defvar doom-modeline-media-player--cache nil)

(defun doom-modeline-media-player--generate-segment ()
  "Generate the mode line segment string."
  (setq doom-modeline-media-player--last-draw-time (float-time))
  (unless doom-modeline-media-player--is-setup
    (doom-modeline-media-player--setup))
  (let ((status (mpris-get-playback-status))
        (metadata (mpris-get-metadata)))
    (unless (and (equal status (caar doom-modeline-media-player--cache))
                 (eq metadata (cdar doom-modeline-media-player--cache)))
      (let* ((format-spec-list
              `((?t . ,(or (mpris-track-attr 'title) ""))
                (?a . ,(or (mpris-track-attr 'artist) ""))
                (?A . ,(or (mpris-track-attr 'album-artist) ""))
                (?r . ,(or (mpris-track-attr 'album) ""))
                (?g . ,(or (mpris-track-attr 'genre) ""))
                (?r . ,(or (mpris-track-attr 'rating) ""))
                (?l . ,(doom-modeline-media-player--format-length
                        (or (mpris-track-attr 'length) 0)))))
             (art
              (and doom-modeline-media-player-show-albumart
                   (when-let ((art-file (mpris-track-attr 'art-file)))
                     (propertize
                      " "
                      'display
                      (list 'image
                            :type (image-type art-file)
                            :file art-file
                            :height (cons doom-modeline-media-player-albumart-height 'em)
                            :ascent 'center)
                      'help-echo
                      (format-spec doom-modeline-media-player-art-hover-text
                                   format-spec-list)))))
             (play-icon
              (and (eq doom-modeline-media-player-playback-indication 'button)
                   (propertize
                    (pcase (mpris-get-playback-status)
                      ("Paused"
                       (doom-modeline-icon 'faicon "nf-fa-play"
                                           "" ">" :v-adjust 0.0575))
                      ("Playing"
                       (doom-modeline-icon 'faicon "nf-fa-pause"
                                           "" "||" :v-adjust 0.0575))
                      ("Stopped"
                       (doom-modeline-icon 'faicon "nf-fa-stop"
                                           "" "x" :v-adjust 0.0575))
                      (_ ""))
                    'mouse-face 'mode-line-highlight
                    'help-echo (format "mouse-1: %s playback"
                                       (pcase (mpris-get-playback-status)
                                         ("Paused" "Resume")
                                         ("Playing" "Pause")
                                         ("Stopped" "Start")
                                         (_ "Toggle")))
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                             #'mpris-play-pause)
                                 map))))
             (track-info
              (propertize
               (truncate-string-to-width
                (format-spec doom-modeline-media-player-display-text format-spec-list)
                doom-modeline-media-player-display-max-width nil nil "…")
               'help-echo (format-spec doom-modeline-media-player-display-hover-text format-spec-list)))
             (face
              (if (or (not (eq doom-modeline-media-player-playback-indication 'dim))
                      (equal (mpris-get-playback-status) "Playing"))
                  'doom-modeline-media-player-text
                'doom-modeline-media-player-text-dim))
             (segment
              (propertize
               (concat
                play-icon (and play-icon (doom-modeline-spc))
                art (and art (doom-modeline-spc))
                track-info)
               'face face)))
        (setq doom-modeline-media-player--cache
              (cons (cons status metadata) segment)))))
  (cdr doom-modeline-media-player--cache))

;;;###autoload (autoload 'doom-modeline-segment--media-player "doom-modeline-media-player")
(doom-modeline-def-segment media-player
  "Media player status, with basic interaction."
  (and (if (functionp doom-modeline-media-player)
           (funcall doom-modeline-media-player)
         doom-modeline-media-player)
       (doom-modeline-media-player--generate-segment)))

(provide 'doom-modeline-media-player)
;;; doom-modeline-media-player.el ends here
