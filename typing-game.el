;;; typing-game.el --- A typing game for Emacs -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Keywords: games

;;; Code:

(require 'request)

(defvar typing-game-buffer-name "*Typing Game*"
  "Name of the typing game buffer.")

(defun typing-game-fetch-random-text ()
  "Fetch a random text from a local directory."
  (let* ((dir (concat (file-name-directory (locate-library "typing-game.el")) "texts"))
         (files (directory-files dir t "^[^.].*\\.txt$"))
         (file (nth (random (length files)) files))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (string-trim-right text)))

(defun typing-game-display-random-text ()
  "Display a random text in the game buffer."
  (let ((text (typing-game-fetch-random-text)))
    (with-current-buffer typing-game-buffer-name
      (let ((inhibit-read-only t)
            (formatted-text "")
            (start 0)
            (end 0)
            (chunk-size 80))
        (while (< end (length text))
          (setq end (min (+ start chunk-size) (length text)))
          (while (and (< end (length text)) (not (equal (aref text end) ?\s)))
            (setq end (- end 1)))
          (when (< end (length text))
            (setq formatted-text (concat formatted-text (substring text start end) " \n"))
            (setq start (+ end 1)))
          (when (>= end (length text))
            (setq formatted-text (concat formatted-text (substring text start)))))
        (erase-buffer)
        (insert formatted-text)
        (goto-char (point-min))))))

(defun typing-game-delete-backward ()
  "Remove the face property from the previous character and move the cursor backward."
  (interactive)
  (let ((inhibit-read-only t))
    (unless (= (point) (point-min))
      (backward-char)
      (remove-text-properties (point) (1+ (point)) '(face nil))
      (setq-local typing-game-wrong-typed nil))))


(defun typing-game-color-typed-text ()
  "Color the typed text in the game buffer."
  (interactive)
  (let ((input (this-command-keys))
        (expected (buffer-substring-no-properties (point) (1+ (point)))))
    (cond
     ((and (not typing-game-wrong-typed) (string= input expected))
      (let ((inhibit-read-only t))
        (let ((bg-color (face-attribute 'default :background)))
          (put-text-property (point) (1+ (point)) 'face
                             `(:foreground ,(if (eq bg-color 'unspecified) "black" bg-color))))
        (forward-char)
        (when (looking-at "\n")
          (forward-char))
        (when (= (point) (point-max))
          (typing-game-display-random-text))))
     (t
      (let ((inhibit-read-only t))
        (if (string= expected " ")
            (put-text-property (point) (1+ (point)) 'face '(:background "red"))
          (put-text-property (point) (1+ (point)) 'face '(:foreground "red")))
        (setq-local typing-game-wrong-typed t)
        (forward-char)
        (when (looking-at "\n")
          (forward-char))
        (when (= (point) (point-max))
          (typing-game-display-random-text)))))))

(defvar typing-game-wrong-typed nil
  "Non-nil if the player has typed a wrong character.")

(defun typing-game-setup-buffer ()
  "Set up the game buffer and key bindings."
  (switch-to-buffer typing-game-buffer-name)
  (setq buffer-read-only t)
  (use-local-map (make-sparse-keymap))
  (cl-loop for c from ?a to ?z do
           (define-key (current-local-map) (char-to-string c) 'typing-game-color-typed-text))
  (cl-loop for c from ?A to ?Z do
           (define-key (current-local-map) (char-to-string c) 'typing-game-color-typed-text))
  (cl-loop for c from ?0 to ?9 do
           (define-key (current-local-map) (char-to-string c) 'typing-game-color-typed-text))
  (dolist (c '(" " "," "." "?" "!" "'" "\"" ";" ":" "(" ")" "[" "]" "{" "}"))
    (define-key (current-local-map) c 'typing-game-color-typed-text))
  (local-unset-key (kbd "DEL"))
  (local-set-key (kbd "DEL") 'typing-game-delete-backward))

;;;###autoload
(defun typing-game ()
  "Start the typing game."
  (interactive)
  (typing-game-setup-buffer)
  (typing-game-display-random-text))

(provide 'typing-game)
