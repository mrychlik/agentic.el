;;; agentic.el --- Agentic LLM workflows in Emacs -*- lexical-binding: t; -*-
;; Author: Marek Rychlik
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (gptel "0.9") (magit "3.3") (forge "0.4") (transient "0.5") (yasnippet "0.14"))
;; Keywords: tools, git, ai
;; URL: https://github.com/mrychlik/agentic.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; agentic.el turns GPT (via gptel) + Magit/Forge into a teachable
;; “prompt → patch → PR” workflow inside Emacs.
;;
;; Highlights:
;; - Rewrite region/buffer using GPT instructions.
;; - Ask GPT for a repo-wide *unified diff* and apply it with git.
;; - Create a safe branch, commit, push, and open a PR via Forge.
;;
;; All external deps (gptel/magit/forge) are *lazy-loaded* so the package
;; can be installed and byte-compiled without them.

;;; Code:

(require 'project) ;; built-in

;; -------------------------------------------------------------------
;; Customization
;; -------------------------------------------------------------------

(defgroup agentic nil
  "Agentic LLM workflows in Emacs."
  :group 'tools
  :prefix "agentic/")

(defcustom agentic/system-prompt
  "You are a careful software engineer. When asked for a patch,
return a *single unified diff* rooted at the project root,
with correct file paths, no extra prose, and no Markdown fences.
Avoid destructive changes; prefer clear, minimal edits."
  "System prompt used when requesting unified diffs."
  :type 'string :group 'agentic)

(defcustom agentic/patch-timeout 90
  "Seconds to wait for a patch response before giving up."
  :type 'integer :group 'agentic)

;; -------------------------------------------------------------------
;; Lazy-loading helpers
;; -------------------------------------------------------------------

(defun agentic--ensure-gptel ()
  "Ensure the `gptel` package is available or signal a friendly error."
  (unless (featurep 'gptel)
    (unless (require 'gptel nil t)
      (user-error "agentic.el: this command needs the `gptel` package (install it)"))))

(defun agentic--ensure-magit ()
  "Ensure the `magit` package is available or signal a friendly error."
  (unless (featurep 'magit)
    (unless (require 'magit nil t)
      (user-error "agentic.el: this command needs the `magit` package (install it)"))))

(defun agentic--ensure-forge ()
  "Ensure the `forge` package (and `magit`) are available."
  (agentic--ensure-magit)
  (unless (featurep 'forge)
    (unless (require 'forge nil t)
      (user-error "agentic.el: this command needs the `forge` package (install it)"))))

;; Optional autoload stubs so byte-compile is quiet when forge isn't present.
(autoload 'forge-get-repository "forge")
(autoload 'forge-add-repository "forge")

;; -------------------------------------------------------------------
;; Utilities
;; -------------------------------------------------------------------

(defun agentic--project-root ()
  "Return the current project root (or error if none)."
  (or (when-let ((pr (project-current t))) (project-root pr))
      (user-error "agentic: not in a project")))

(defun agentic--call-process (program &rest args)
  "Call PROGRAM synchronously with ARGS; raise a user-error on non-zero exit."
  (let* ((buf (generate-new-buffer " *agentic-cmd*"))
         (status (apply #'process-file program nil buf t args))
         (out (with-current-buffer buf (buffer-string))))
    (unwind-protect
        (if (eq status 0) out
          (user-error "agentic: `%s %s` failed:\n%s" program (string-join args " ") out))
      (kill-buffer buf))))

;; -------------------------------------------------------------------
;; Keymap & minor mode
;; -------------------------------------------------------------------

(defvar agentic-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c g r") #'agentic/gpt-rewrite)
    (define-key m (kbd "C-c g p") #'agentic/gpt-patch-apply)
    (define-key m (kbd "C-c g P") #'agentic/gpt-patch-preview)
    (define-key m (kbd "C-c g b") #'agentic/git-make-safe-branch)
    (define-key m (kbd "C-c g c") #'agentic/git-commit-all)
    (define-key m (kbd "C-c g u") #'agentic/git-push-current)
    (define-key m (kbd "C-c g R") #'agentic/forge-open-pr)
    (define-key m (kbd "C-c g C") #'agentic/gpt-compose-patch)
    (define-key m (kbd "C-c g W") #'agentic/gpt-compose-rewrite)
    m)
  "Keymap for `agentic-mode`.")

;;;###autoload
(define-minor-mode agentic-mode
  "Agentic LLM workflows in Emacs.

When enabled, binds `C-c g` to the main commands:
- `agentic/gpt-rewrite`      — rewrite region/buffer with GPT.
- `agentic/gpt-patch-apply`  — request unified diff for the repo and apply it.
- `agentic/gpt-patch-preview`— preview the unified diff without applying.
- `agentic/git-*`            — branch/commit/push helpers.
- `agentic/forge-open-pr`    — open a PR via Forge.

All GPT/Magit/Forge deps are lazy-loaded at call time."
  :lighter " Agentic"
  :keymap agentic-mode-map)

;;;###autoload
(define-globalized-minor-mode global-agentic-mode
  agentic-mode
  (lambda () (agentic-mode 1)))

;; -------------------------------------------------------------------
;; GPT: rewrite / compose
;; -------------------------------------------------------------------

;;;###autoload
(defun agentic/gpt-rewrite (instruction)
  "Rewrite region or buffer using INSTRUCTION via GPT (gptel).

If a region is active, only that region is rewritten; otherwise the
entire buffer is rewritten *in place*. You can always undo if needed."
  (interactive "sRewrite instruction: ")
  (agentic--ensure-gptel)
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end)       (point-max)))
         (original (buffer-substring-no-properties beg end))
         (prompt (format "Rewrite the following content per instruction.\nInstruction:\n%s\n\nContent:\n%s"
                         instruction original))
         (response (gptel-prompt prompt))) ;; simple synchronous call
    (when (and response (not (string-empty-p response)))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert response))
      (message "agentic: rewrite applied."))))

;;;###autoload
(defun agentic/gpt-compose-rewrite ()
  "Open a scratch buffer to compose a multi-line rewrite instruction.

Use `C-c C-c` to submit (calls `agentic/gpt-rewrite` with the text),
or `C-c C-k` to cancel."
  (interactive)
  (let ((buf (get-buffer-create "*Agentic Rewrite*")))
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let ((instr (string-trim (buffer-string))))
                         (unless (string-empty-p instr)
                           (kill-buffer buf)
                           (call-interactively #'agentic/gpt-rewrite)))))
      (local-set-key (kbd "C-c C-k") #'kill-current-buffer)
      (insert "Describe how to rewrite the active region or the whole buffer…\n"))
    (pop-to-buffer buf)))

;; -------------------------------------------------------------------
;; GPT: unified diff (preview/apply) + compose
;; -------------------------------------------------------------------

(defun agentic--patch-system-prompt ()
  "Return the system prompt string for patch requests."
  agentic/system-prompt)

(defun agentic--project-root-or-error ()
  (or (agentic--project-root)
      (user-error "agentic: cannot determine project root")))

(defun agentic--ask-gpt-for-diff (user-prompt)
  "Return a unified diff string from GPT given USER-PROMPT."
  (agentic--ensure-gptel)
  (let ((prompt (format
                 "You are an expert code editor. %s\n\nProject root: %s\n\nUser request:\n%s"
                 (agentic--patch-system-prompt)
                 (agentic--project-root-or-error)
                 user-prompt)))
    (gptel-prompt prompt))) ;; keep simple; users can swap streaming, etc.

;;;###autoload
(defun agentic/gpt-patch-preview (prompt)
  "Ask GPT for a unified diff (for the current project) and show it in a buffer.

PROMPT is a natural-language instruction. The diff is *not* applied."
  (interactive "sPatch prompt: ")
  (let ((diff (agentic--ask-gpt-for-diff prompt)))
    (if (string-match-p "^\\s-*$" (or diff ""))
        (user-error "agentic: GPT returned an empty response")
      (with-current-buffer (get-buffer-create "*Agentic Diff*")
        (erase-buffer)
        (insert diff)
        (diff-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun agentic/gpt-patch-apply (prompt)
  "Ask GPT for a unified diff (for the current project) and apply it with git.

This runs `git apply` in the project root. On failure, `.rej` files are
left behind; open Magit to resolve conflicts."
  (interactive "sPatch prompt: ")
  (let* ((root (agentic--project-root-or-error))
         (default-directory root)
         (diff (agentic--ask-gpt-for-diff prompt)))
    (when (string-match-p "^\\s-*$" (or diff ""))
      (user-error "agentic: GPT returned an empty response"))
    ;; write the diff to a temp file and apply
    (let ((tmp (make-temp-file "agentic-diff-" nil ".patch")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert diff))
            (agentic--call-process "git" "apply" "--reject" "--whitespace=nowarn" tmp)
            (message "agentic: patch applied. Review changes in Magit if desired."))
        (ignore-errors (delete-file tmp))))))

;;;###autoload
(defun agentic/gpt-compose-patch ()
  "Open a buffer to compose a PATCH prompt for GPT.

Use `C-c C-c` to submit (preview the diff), then `C-c g p` to apply
if you are satisfied."
  (interactive)
  (let ((buf (get-buffer-create "*Agentic Patch*")))
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let ((p (string-trim (buffer-string))))
                         (unless (string-empty-p p)
                           (kill-buffer buf)
                           (call-interactively #'agentic/gpt-patch-preview)))))
      (local-set-key (kbd "C-c C-k") #'kill-current-buffer)
      (insert "Describe the desired changes. GPT will return a single unified diff.\n"))
    (pop-to-buffer buf)))

;; -------------------------------------------------------------------
;; Git helpers (branch/commit/push)
;; -------------------------------------------------------------------

;;;###autoload
(defun agentic/git-make-safe-branch (prefix)
  "Create a new branch named PREFIX/YYYY-MM-DD, starting from the current branch."
  (interactive "sBranch prefix: ")
  (agentic--ensure-magit)
  (let* ((date (format-time-string "%Y-%m-%d"))
         (name (format "%s/%s" (string-trim prefix) date)))
    (magit-branch-and-checkout name (magit-get-current-branch))
    (message "agentic: switched to branch %s" name)))

;;;###autoload
(defun agentic/git-commit-all (msg)
  "Commit *all* changes in the project with message MSG."
  (interactive "sCommit message: ")
  (agentic--ensure-magit)
  (let ((default-directory (agentic--project-root-or-error)))
    (agentic--call-process "git" "add" "-A")
    (agentic--call-process "git" "commit" "-m" msg)
    (message "agentic: committed changes.")))

;;;###autoload
(defun agentic/git-push-current ()
  "Push the current branch to origin; set upstream if needed."
  (interactive)
  (agentic--ensure-magit)
  (let ((branch (magit-get-current-branch)))
    (agentic--call-process "git" "push" "--set-upstream" "origin" branch)
    (message "agentic: pushed %s to origin." branch)))

;; -------------------------------------------------------------------
;; Forge: open PR
;; -------------------------------------------------------------------

;;;###autoload
(defun agentic/forge-open-pr (title body)
  "Open a GitHub pull request for the current branch via Forge.

TITLE and BODY are used to populate the PR. Ensure `forge` is configured
for this repository (`M-x forge-add-repository` first time)."
  (interactive "sPR title: \nsPR body: ")
  (agentic--ensure-forge)
  (let ((repo (or (forge-get-repository t)
                  (progn (call-interactively #'forge-add-repository)
                         (forge-get-repository t)))))
    (unless repo (user-error "agentic: no Forge repository configured"))
    (magit-push-current-to-pushremote nil)
    (forge-create-pullreq repo title body)
    (message "agentic: PR created (check your browser or Forge buffer).")))

(provide 'agentic)
;;; agentic.el ends here
