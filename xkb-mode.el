;;; xkb-mode.el --- Major mode for editing X Keyboard Extension (XKB) files
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/captainflasmr/xkb-mode
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;; xkb-mode provides syntax highlighting and other editing features
;; for working with XKB files.
;;
;;; Alternatives.
;;
;; As far as I can tell this is the first Emacs minor mode supporting the xkb format.

;;; Code:

(require 'regexp-opt)

(defun xkb-indent-line ()
  "Indent current line as XKB code."
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check if it's the beginning of the buffer
      (indent-line-to 0)  ; Set indent to 0 at the start of the buffer.
    (let ((not-indented t) (cur-indent nil))
      ; If the line starts with '}', potentially reduce indentation.
      (if (looking-at "^[ \t]*}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (when (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          ; Move up through the lines to find context for indentation.
          (while not-indented
            (forward-line -1)
            (cond
             ((looking-at "^.*[ \t]*}") ; Matches a closing block.
              (setq cur-indent (current-indentation))
              (setq not-indented nil))
             ((looking-at "^.*[ \t]*{") ; Matches an opening block, increase indent.
              (setq cur-indent (+ (current-indentation) tab-width))
              (setq not-indented nil))
             ((bobp)  ; Reached the beginning of the buffer.
              (setq not-indented nil))
             ((and (not cur-indent) (not (looking-at "^.*[ \t]*$"))) ; Non-blank line without prior context
              (setq cur-indent (current-indentation)))))))
      (indent-line-to (or cur-indent (current-indentation) 0)))))

;;;###autoload
(define-derived-mode xkb-mode prog-mode "XKB"
  "Major mode for editing XKB keyboard layout files."
  (kill-all-local-variables)
  ;; Syntax highlighting
  (setq font-lock-defaults
    `((
        ;; Comments
        ("\\(#.*\\)" . font-lock-comment-face)
        ;; Keywords and Sections
        (,(regexp-opt '("xkb_keycodes" "xkb_keymap" "xkb_types" "xkb_compatibility" "xkb_symbols" "xkb_geometry"
                         "useModMapMods" "virtual_modifiers" "type" "interpret" "action" "include" "name" "group"
                         "indicator" "modifiers" "map" "level_name" "alias" "key" "modifier_map"
                         "virtualModifier" "preserve" "repeat" "symbols" "virtual" "section") 'words) . font-lock-keyword-face)
        ;; Function names and important identifiers
        (,(regexp-opt '("AnyOf" "AllOf" "NoneOf" "AnyOfOrNone" "LatchMods" "LockMods" "SetMods"
                         "LatchGroup" "LockGroup" "SetGroup" "MovePtr" "PtrBtn" "LockPtrBtn"
                         "SetPtrDflt" "ISO_Lock" "ISO_Level2_Latch" "ISO_Level3_Shift" "ISO_Level3_Latch"
                         "ISO_Level3_Lock" "ISO_Level5_Shift" "ISO_Level5_Latch" "ISO_Level5_Lock"
                         "SetControls" "LockControls" "ActionMessage" "RedirectKey" "DeviceBtn" "LockDeviceBtn"
                         "AccessX" "Terminate" "SwitchScreen" "SetModMap" "SetExplicit" "ModMap" "VirtualMods"
                         "Interpret" "SymInterp" "vmods" "type" "key" "overlay1" "overlay2"
                         "outline" "solid" "text" "row" "keys" "keyType") 'words) . font-lock-function-name-face)
        ;; Pattern to match type declarations like 'type= "FOUR_LEVEL"'
        ("\\<type\\s-*=\\s-*\"\\([^\"]*\\)\"" 1 font-lock-type-face)
        ;; Pattern to highlight the symbols specification, capturing the property name and its list value
        ("\\<symbols\\[Group[[:digit:]]+\\]\\s-*=\\s-*\\[\\(\\s-*[[:alnum:][:space:],_]+\\s-*\\)\\]" 1 font-lock-constant-face)
        ;; Modifiers and constants
        (,(concat
            "\\<\\("
            "Group[[:digit:]]*\\|"
            "ISO_\\w+*\\|"
            "KP_\\w+*\\|"
            "Hyper_\\w*\\|"
            "Control_\\w*\\|"
            "XF86[_[a-zA-Z0-9]*\\|"
            "Meta_\\w*\\|"
            "[0-9]+\\|"
            "Alt_\\w*\\|"
            "Shift_\\w*\\|"
            "Super_\\w*\\"
            ")\\>"
            "\\|"
            (regexp-opt '("Shift" "Lock" "Control" "Mod1" "Mod2" "Mod3" "Mod4" "Mod5"
                           "plusminus" "Pause" "Break" "Menu" "Cancel" "Multi_key"
                           "baseColor" "labelColor" "xfont " "description" "xfont"
                           "width" "height" "shape" "top" "left" "priority" "onColor" "offColor" "XFont"
                           "Redo" "SunProps" "Undo" "SunFront" "Find" "Help"
                           "parenright" "parenleft" "NoSymbol"
                           "Print" "Sys_Req" "Linefeed" "Home" "Up" "Prior"
                           "Left" "Right" "End" "Down" "Next" "Insert" "Delete"
                           "NumLock" "CapsLock" "ScrollLock" "ShiftLock"
                           "Num_Lock" "Caps_Lock" "Scroll_Lock" "space") 'words)) . font-lock-constant-face)
        ;; Special strings and constants
        ("\\(\"[^\"]+\"\\)" . font-lock-string-face)
        ;; Key names (e.g., <ESC>, <AE01>, etc.), potentially a large range so kept broad
        (,(concat
            "\\<\\("
            "\\<[<>A-Za-z0-9]+\\>\\|"
            "symbols\\|"
            "type\\"
            ")\\>") . font-lock-variable-name-face))))
  ;; Comments
  (setq comment-start "#")
  (setq comment-end "")
  ;; Indentation
  (setq indent-tabs-mode nil)
  (setq indent-line-function 'xkb-indent-line)
  ;; Key bindings
  ;; Here we could add key bindings specific to XKB editing if necessary
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xkb\\'" . xkb-mode))

(provide 'xkb-mode)

;;; xkb-mode.el ends here
