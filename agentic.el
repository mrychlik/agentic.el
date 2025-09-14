;;; agentic.el --- Agentic LLM workflows in Emacs -*- lexical-binding: t; -*-
;; Author: Marek Rychlik (with AI assist)
;; Version: 0.2
;; Keywords: tools, git, ai
;; Package-Requires: ((emacs "27.1") (gptel "0.9") (magit "3.3") (forge "0.4") (transient "0.5") (yasnippet "0.14"))
;; URL: https://github.com/mrychlik/agentic.el
;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Agentic editing in Emacs with GPT (via gptel) + Magit/Forge:
;; - Rewrite buffers/regions.
;; - Request a unified diff for the repo and apply with git.
;; - Create a safe branch, commit, push, and open a PR via Forge.
;; Keep OPENAI_API_KEY in your shell env. First time per repo with Forge:
;; M-x forge-add-repository (authenticate to GitHub).

;;; Code:
;;; Commentary:
;; A light module that wires GPT-5 (via gptel) into an Emacs/Magit workflow:
;; - Ask GPT-5 to rewrite a region/buffer.
;; - Ask GPT-5 for a unified diff and apply it with git.
;; - Create a safe branch, commit, push, and open a PR (Forge).
;;
;; Keep OPENAI_API_KEY in your shell env, not in Emacs files.
;; First time per repo with Forge: M-x forge-add-repository (auth to GitHub).

;;; Code:

(require 'package)
;;(require 'ghub)   ;; for GitHub API calls (used by Forge)
(unless (require 'ghub nil t)
  (user-error "Package 'ghub' is required for opening PRs (install ghub)."))

(require 'gptel)
(require 'magit)
;;(require 'forge)
(autoload 'forge-get-repository "forge")
(autoload 'forge-add-repository "forge")
(require 'project)

;; Pick a model string your gptel supports right now.
;; If "gpt-5" isn’t recognized in your build, test with one of these and switch later:
; (setq gptel-model "gpt-4o-mini")    ;; very fast
;; (setq gptel-model "gpt-4.1")     ;; or this, if you prefer
;; (setq gptel-model "gpt-4o")      ;; also fine


;; --- gptel backend + model (working settings) ---
(require 'gptel)
(setq gptel-log-level 'debug)

(setq gptel-backend
      (gptel-make-openai "OpenAI"
        :host "api.openai.com"
        :endpoint "/v1/chat/completions"
        ;; Map model symbols -> API strings so gptel won’t send "nil"
	:models '((gpt-5         . "gpt-5")
		  (gpt-5-mini    . "gpt-5-mini")
		  (gpt-4o-mini   . "gpt-4o-mini")
		  (gpt-4o        . "gpt-4o")
		  (gpt-4.1       . "gpt-4.1")
		  (gpt-4.1-mini  . "gpt-4.1-mini"))
        :stream t
        :key (lambda ()
               (or (getenv "OPENAI_API_KEY")
                   (auth-source-pick-first-password
                    :host "api.openai.com" :user "OPENAI_API_KEY")))))

;; IMPORTANT: current gptel expects a SYMBOL here
;;(setq gptel-model 'gpt-4.1-mini)  ;; start safe; switch to 'gpt-4o-mini later if allowed
(setq gptel-model 'gpt-5-mini)

;; -- Customization --------------------------------------------------
(defgroup agentic nil
  "GPT-5 + Magit/Forge helpers."
  :group 'agentic)

(defcustom agentic/gpt-system-prompt
  "You are a senior engineer and applied mathematician. Return only the revised code/text unless asked for a diff."
  "System instruction for rewrite requests."
  :type 'string :group 'agentic)

;; -- Utilities ------------------------------------------------------
(defun agentic/project-root ()
  "Best-effort project root."
  (or (when (fboundp 'project-current)
        (when-let* ((proj (project-current nil)))
          (project-root proj)))
      (locate-dominating-file default-directory ".git")
      default-directory))

;; -- 1) Rewrite region/buffer with GPT-5 ----------------------------
;;; Replace the whole agentic/gpt-rewrite with this:
;;;###autoload
(defun agentic/gpt-rewrite (instruction)
  "Rewrite the active region (or whole buffer) per INSTRUCTION using GPT-5."
  (interactive "sRewrite instruction: ")
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end)       (point-max)))
         (original (buffer-substring-no-properties beg end))
         (prompt (format
                  (concat "Rewrite the text between BEGIN/END according to the instruction.\n"
                          "Instruction: %s\n\nBEGIN\n%s\nEND\n")
                  instruction original)))
    (gptel-request
     prompt
     :system agentic/gpt-system-prompt
     :callback
     (lambda (resp _info)
       (unless (and resp (stringp resp))
         (user-error "No response from model."))
       (save-excursion
         (delete-region beg end)
         (goto-char beg)
         (insert resp))
       (message "Applied GPT-5 edit.")))))

;; -- 2) Ask GPT-5 for a unified diff and apply it -------------------

;;;; Ergonomic “ask for patch → auto-apply” (no buffer overwrite)

;;;###autoload
(defvar agentic/gpt-last-patch-file nil
  "Path to the last patch written by agentic/gpt-patch-apply.")

(defun agentic--strip-code-fences (s)
  "Remove Markdown code fences from S if present."
  (let ((str s))
    (when (string-match "\\`[ \t\r\n]*```\\(?:[[:alpha:]-]+\\)?[ \t\r\n]*" str)
      (setq str (substring str (match-end 0))))
    (when (string-match "[ \t\r\n]*```[ \t\r\n]*\\'" str)
      (setq str (substring str 0 (match-beginning 0))))
    str))

(defun agentic--require-diff (s)
  "Return substring of S starting at first unified-diff header, or error."
  (let* ((rx "^diff --git[ \t]+a/")
         (idx (string-match rx s)))
    (unless idx
      (user-error "Model did not return a unified diff (missing 'diff --git a/...')."))
    (substring s idx)))

(defun agentic/project-root ()
  (or (ignore-errors (project-root (project-current)))
      (locate-dominating-file default-directory ".git")
      default-directory))

;;;###autoload
(defun agentic/gpt-patch-apply (prompt)
  "Ask the model for a unified diff and auto-apply it to the repo.
No preview buffer; uses git apply with safety checks."
  (interactive "sGPT patch request: ")
  (let* ((root (file-name-as-directory (agentic/project-root)))
         (sys (format
               (concat "Return ONE unified diff for repo root: %s\n"
                       "- Headers: diff --git a/… b/… ; include ---/+++ and @@ context\n"
                       "- New files must use 'new file mode' with /dev/null in --- line\n"
                       "- ABSOLUTELY NO prose or code fences, diff only.\n")
               root)))
    (gptel-request
     (format "TASK: %s\n\nNOTE: repo root is %s" prompt root)
     :system sys
     :callback
     (lambda (resp info)
       (unless (and resp (stringp resp))
         (user-error "No patch text returned: %s" (plist-get info :error)))
       (setq resp (agentic--strip-code-fences resp))
       (setq resp (agentic--require-diff resp))
       (let ((patch-file (make-temp-file "gpt5-" nil ".patch")))
         (setq agentic/gpt-last-patch-file patch-file)
         (with-temp-file patch-file (insert resp))
         (let ((default-directory root))
           ;; Try clean apply, then whitespace fix, then 3-way, then reject.
           (cond
            ((eq 0 (magit-call-git "apply" "--check" patch-file))
             (magit-call-git "apply" patch-file)
             (message "Patch applied ✓  (%s)\nNext: C-c g c to commit, C-c g u to push."
                      (file-name-nondirectory patch-file)))
            ((eq 0 (magit-call-git "apply" "--check" "--whitespace=fix" patch-file))
             (magit-call-git "apply" "--whitespace=fix" patch-file)
             (message "Patch applied with whitespace fix ✓  (%s)\nNext: C-c g c, then C-c g u."
                      (file-name-nondirectory patch-file)))
            ((eq 0 (magit-call-git "apply" "--3way" "--check" patch-file))
             (magit-call-git "apply" "--3way" patch-file)
             (message "Patch applied with 3-way merge ✓  (%s)\nReview changes, then C-c g c."
                      (file-name-nondirectory patch-file)))
            (t
             (magit-call-git "apply" "--reject" patch-file)
             (pop-to-buffer "*magit-process*")
             (user-error "Patch conflicted → .rej files created. Resolve & stage manually.")))))))))

;; Optional: a “preview” variant if you ever want to inspect raw diff.
;;;###autoload
(defun agentic/gpt-patch-preview (prompt)
  "Ask for a unified diff and show it in a temp buffer (no apply)."
  (interactive "sGPT patch request (preview): ")
  (let* ((root (file-name-as-directory (agentic/project-root)))
         (sys (format
               (concat "Return ONE unified diff for repo root: %s\n"
                       "NO prose, NO code fences, diff only.\n")
               root)))
    (gptel-request
     (format "TASK: %s\n\nNOTE: repo root is %s" prompt root)
     :system sys
     :callback
     (lambda (resp info)
       (unless (and resp (stringp resp))
         (user-error "No patch text returned: %s" (plist-get info :error)))
       (let ((buf (get-buffer-create "*GPT Patch Preview*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert (agentic--require-diff (agentic--strip-code-fences resp)))
           (diff-mode))
         (pop-to-buffer buf)
         (message "Preview only. To apply: git apply %s (last saved)."
                  (or agentic/gpt-last-patch-file "the saved patch")))))))


;; -- 3) Magit helpers: safe branch / commit / push ------------------
;;;###autoload
(defun agentic/git--safe-branch-name (prefix)
  "Make a safe, timestamped branch name from PREFIX."
  (let* ((ts (format-time-string "%Y%m%d-%H%M%S" (current-time) t))
         (base (or (magit-get-current-branch) "main"))
         (p (string-trim (or prefix "")))
         (p (if (member p (list base "main" "master"))
                (format "ai-%s" p) p))
         (raw (if (string-empty-p p) ts (format "%s-%s" p ts)))
         (safe (replace-regexp-in-string "[/ ~]+" "-" raw)))
    safe))

;;;###autoload
(defun agentic/git-make-safe-branch (prefix)
  "Create and switch to a safe, timestamped branch from PREFIX (no
interactive Magit calls)."
  (interactive "sBranch prefix (e.g., ai/patch): ")
  (let* ((ts (format-time-string "%Y%m%d-%H%M%S" (current-time) t))
         (base (or (magit-get-current-branch) "main"))
         (p (string-trim (or prefix "")))
         (p (if (member p (list base "main" "master")) (format "ai-%s" p) p))
         (raw (if (string-empty-p p) ts (format "%s-%s" p ts)))
         (branch (replace-regexp-in-string "[/ ~]+" "-" raw)))
    ;; git checkout -b <branch> <base>
    (magit-call-git "checkout" "-b" branch base)
    (message "Switched to new branch: %s" branch)))
;;;###autoload

(defun agentic/git-commit-all (msg)
  "Stage all changes and commit with MSG using plumbing."
  (interactive "sCommit message: ")
  ;; Stage new/modified/deleted files
  (magit-call-git "add" "-A")
  ;; Commit
  (if (= 0 (magit-call-git "commit" "-m" msg))
      (message "Committed: %s" msg)
    (user-error "Nothing to commit (or commit failed)")))

;;;###autoload
(defun agentic/git-push-current ()
  "Push current branch, setting upstream if needed (plumbing only)."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (remote (or (magit-get "branch" branch "remote")
                     (magit-get "remote.pushDefault")
                     "origin")))
    (unless branch (user-error "Not on a branch"))
    (if (magit-get-upstream-branch branch)
        ;; upstream exists → plain push
        (magit-call-git "push" remote branch)
      ;; no upstream → set upstream
      (magit-call-git "push" "--set-upstream" remote branch))
    (message "Pushed %s -> %s" branch remote)))


(defun agentic/forge-ensure-auth ()
  (unless (file-exists-p (expand-file-name "~/.authinfo.gpg"))
    (user-error "No ~/.authinfo.gpg found. Create a GitHub token and store it there (see instructions)."))
  (let ((repo (forge-get-repository :tracked)))
    (unless repo
      (user-error "Forge isn’t tracking this repo yet. Run M-x forge-add-repository.")))
  t)

;; -- 4) Forge: open a PR from Emacs --------------------------------
;;;###autoload
(defun agentic/forge-open-pr (title body)
  "Open a PR for the current branch against the default branch using GitHub
API via ghub."
  (interactive "sPR title: \nsPR body: ")
  (agentic/forge-ensure-auth)
  (let* ((repo (forge-get-repository :tracked)))
    (unless repo
      (user-error "Forge isn’t tracking this repo yet. Run M-x forge-add-repository."))
    (require 'ghub)
    (let* ((owner  (oref repo owner))
           (name   (oref repo name))
           (base   (or (oref repo default-branch) "main"))
           (branch (magit-get-current-branch)))
      (unless branch (user-error "Not on a branch"))
      (agentic/git-push-current) ;; ensure branch is on origin
      (let* ((payload `((title . ,title)
                        (head  . ,(format "%s:%s" owner branch))
                        (base  . ,base)
                        (body  . ,body)))
             (resp (ghub-post (format "/repos/%s/%s/pulls" owner name)
                              nil :payload payload :auth 'forge)))
        (let ((html-url (alist-get 'html_url resp)))
          (message "Opened PR: %s" html-url)
          (when html-url (browse-url html-url)))))))

;;; Probe which OpenAI models this project key can use (no fallbacks added).
;;;###autoload
(defun agentic/gptel-probe-models (&optional models)
  "Probe MODELS (symbols) with a tiny request via current gptel-backend.
Show results table in *gptel-probe* buffer (OK or API error)."
  (interactive)
  (require 'gptel)
  (let* ((candidates (or models
                         '(gpt-4o-mini gpt-4o gpt-4.1 gpt-4.1-mini gpt-4 gpt-3.5-turbo)))
         (results '())
         (start-time (float-time)))
    (cl-labels
        ((step (ms)
               (if (null ms)
                   ;; Done → render results
                   (let ((buf (get-buffer-create "*gptel-probe*")))
                     (with-current-buffer buf
                       (read-only-mode -1)
                       (erase-buffer)
                       (insert (format "gptel backend: %s\n"
                                       (ignore-errors (gptel-backend-name gptel-backend))))
                       (insert (format "tested in %.2fs\n\n" (- (float-time) start-time)))
                       (insert (format "%-14s  %s\n" "MODEL" "RESULT"))
                       (insert (make-string 60 ?-) "\n")
                       (dolist (r (nreverse results))
                         (pcase-let ((`(,m . ,st) r))
                           (insert (format "%-14s  %s\n" m
                                           (if (eq st 'ok) "OK"
                                             (format "%s" st))))))
                       (goto-char (point-min))
                       (read-only-mode 1))
                     (pop-to-buffer buf))
                 ;; Else try next
                 (let ((model (car ms)))
                   (let ((gptel-model model))
                     (gptel-request
                      "ping"
                      :callback
                      (lambda (resp info)
                        (push (cons model (if (stringp resp) 'ok
                                            (or (plist-get info :error) 'error)))
                              results)
                        (step (cdr ms)))))))))
      (message "Probing models… %S" candidates)
      (step candidates))))

;;;###autoload
(defun agentic/yas-expand-by-key (key &optional mode)
  "Expand yasnippet with KEY for MODE (defaults to `major-mode')."
  (interactive "sSnippet key: ")
  (require 'yasnippet)
  (let* ((mode (or mode major-mode))
         (tmpl (or (yas-lookup-snippet key mode)
                   (progn (yas-reload-all)
                          (yas-lookup-snippet key mode)))))
    (unless tmpl
      (user-error "Snippet with key %S not found for mode %S" key mode))
    (yas-expand-snippet tmpl)))


;;;; GPT request composer buffers (multi-line, yas-enabled)

(defvar agentic/gpt-compose-buffer-name "*GPT Compose*")
(defvar agentic/gpt-compose-kind nil)  ;; 'patch or 'rewrite (buffer-local)
(put 'agentic/gpt-compose-kind 'permanent-local t)

(define-minor-mode agentic/gpt-compose-mode
  "Minor mode for composing GPT requests (patch/rewrite)."
  :lighter " GPT-Compose"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-c C-c") #'agentic/gpt-compose-submit)
            (define-key m (kbd "C-c C-k") #'agentic/gpt-compose-cancel)
            m))

(defun agentic/gpt--compose-pop (title kind initial)
  "Open a compose buffer titled TITLE for KIND with INITIAL text."
  (let* ((buf (get-buffer-create agentic/gpt-compose-buffer-name)))
    (pop-to-buffer buf)
    (erase-buffer)
    (insert initial)
    (goto-char (point-min))
    (markdown-mode)               ;; or gfm-mode, if you prefer
    (agentic/gpt-compose-mode 1)
    (setq-local agentic/gpt-compose-kind kind)
    (setq-local header-line-format title)
    (when (fboundp 'yas-minor-mode) (yas-minor-mode 1))
    (message "Edit prompt. C-c C-c to send, C-c C-k to cancel.")))

;;;###autoload
(defun agentic/gpt-compose-patch ()
  "Open a buffer to compose a PATCH prompt (yas + multi-line)."
  (interactive)
  (agentic/gpt--compose-pop
   "GPT PATCH — C-c C-c to submit"
   'patch
   (concat
    "CONTRACT: \n\n"
    "REQUIREMENTS:\n"
    "- Create/modify only the intended paths\n"
    "- Keep patch small and focused\n"
    "- Return ONLY a raw unified diff (no prose, no fences)\n\n"
    "diff --git a/ b/\n"
    "new file mode 100644\n"
    "--- /dev/null\n"
    "+++ b/\n"
    "@@\n")))

;;;###autoload
(defun agentic/gpt-compose-rewrite ()
  "Open a buffer to compose a REWRITE prompt (yas + multi-line)."
  (interactive)
  (agentic/gpt--compose-pop
   "GPT REWRITE — C-c C-c to submit"
   'rewrite
   (concat
    "Rewrite the selected code following this guidance:\n\n"
    "- Keep behavior the same (unless specified)\n"
    "- Improve clarity, add docstring, keep style consistent\n"
    "- Return only RAW code (no markdown fences or prose)\n\n")))

;;;###autoload
(defun agentic/gpt-compose-cancel ()
  (interactive)
  (kill-buffer agentic/gpt-compose-buffer-name)
  (message "Canceled."))

;;;###autoload
(defun agentic/gpt-compose-submit ()
  "Submit the composed request using your existing GPT functions."
  (interactive)
  (let* ((prompt (buffer-substring-no-properties (point-min) (point-max)))
         (kind agentic/gpt-compose-kind))
    (unless (and prompt (> (length (string-trim prompt)) 0))
      (user-error "Prompt is empty."))
    (pcase kind
      ('patch   (agentic/gpt-patch-apply prompt))   ;; uses your auto-apply flow
      ('rewrite (agentic/gpt-rewrite prompt))       ;; call your rewrite function
      (_ (user-error "Unknown compose kind: %S" kind)))))

(require 'magit)
(require 'tabulated-list)

;; ---------- Top authors (commits) ----------
(defun agentic/git-top-authors (&optional since branch limit)
  "Show top authors by commit count.
SINCE like \"1.year\" or \"2025-01-01\". BRANCH defaults to HEAD.
LIMIT is number of rows to show (default 20)."
  (interactive
   (list (read-string "Since (e.g. 1.year / 2025-01-01, empty=all): " nil nil "")
         (read-string "Branch/range (empty=HEAD): " nil nil "")
         (let ((n (read-number "How many authors to show: " 20))) n)))
  (let* ((default-directory (or (magit-toplevel) default-directory))
         (range (if (string-empty-p branch) "HEAD" branch))
         (args (append '("shortlog" "-sne")
                       (unless (string-empty-p since) (list (concat "--since=" since)))
                       (list range "--")))
         (lines (magit-git-lines "git" args))
         (rows '())
         (total 0))
    (dolist (ln lines)
      ;; format: "  123\tName <email>"
      (when (string-match "\\`[[:space:]]*\\([0-9]+\\)\\t\\(.+\\)" ln)
        (let ((n (string-to-number (match-string 1 ln)))
              (who (match-string 2 ln)))
          (setq total (+ total n))
          (push (list who (vector (number-to-string n) who)) rows))))
    (setq rows (nreverse rows))
    (let* ((rows (if limit (cl-subseq rows 0 (min limit (length rows))) rows))
           (buf (get-buffer-create "*Git Top Authors*")))
      (with-current-buffer buf
        (agentic--tablist-mode "Top Authors (commits)"
                          '(("Commits" 10 t) ("Author" 50 t))
                          rows)
        (tabulated-list-print t)
        (goto-char (point-min))
        (forward-line 2)
        (insert (format "Total commits%s: %d\n"
                        (if (string-empty-p since) "" (format " since %s" since))
                        total))
        (dolist (it rows)
          (let* ((n (string-to-number (aref (cadr it) 0)))
                 (pct (* 100.0 (/ (float n) (max 1 total)))))
            (insert (format "  %-48s %4d  (%5.1f%%)\n" (car it) n pct))))
        (goto-char (point-min)))
      (pop-to-buffer buf))))

(defun agentic--tablist-mode (title columns rows)
  "Internal helper to show ROWS in tabulated-list."
  (setq-local tabulated-list-format (vconcat (mapcar (lambda (c)
                                                       (list (nth 0 c) (nth 1 c) (nth 2 c)))
                                                     columns)))
  (setq-local tabulated-list-entries (lambda () rows))
  (tabulated-list-mode)
  (setq header-line-format title))

;; ---------- Bus factor (≈ authors covering ~80% commits) ----------
;;;###autoload
(defun agentic/git-bus-factor (&optional since branch threshold)
  "Approximate bus factor: minimal authors covering THRESHOLD (default 0.8) of commits."
  (interactive
   (list (read-string "Since (empty=all): " nil nil "")
         (read-string "Branch/range (empty=HEAD): " nil nil "")
         (read-number "Threshold (0.8=80%%): " 0.8)))
  (let* ((default-directory (or (magit-toplevel) default-directory))
         (range (if (string-empty-p branch) "HEAD" branch))
         (args (append '("shortlog" "-sne")
                       (unless (string-empty-p since) (list (concat "--since=" since)))
                       (list range "--")))
         (counts (mapcar (lambda (ln)
                           (when (string-match "\\`[[:space:]]*\\([0-9]+\\)\\t\\(.+\\)" ln)
                             (string-to-number (match-string 1 ln))))
                         (magit-git-lines "git" args)))
         (sorted (sort (delq nil counts) #'>))
         (total (apply #'+ sorted))
         (goal (* (or threshold 0.8) total))
         (acc 0) (k 0))
    (while (and sorted (< acc goal))
      (setq acc (+ acc (car sorted))
            sorted (cdr sorted)
            k (1+ k)))
    (message "Bus factor ~%d (covers %.1f%% of %d commits%s)"
             k (if (> total 0) (* 100.0 (/ (float acc) total)) 0.0) total
             (if (string-empty-p since) "" (format " since %s" since)))))

;; ---------- File ownership (blame shares) ----------
;;;###autoload
(defun agentic/git-file-owners (&optional file)
  "Show blame-based line ownership for FILE (default current buffer)."
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (default-directory (or (magit-toplevel) default-directory)))
    (unless (and file (file-exists-p file))
      (user-error "No file to analyze"))
    (let* ((lines (magit-git-lines "blame" "--line-porcelain" "--" file))
           (authors '()) (total 0))
      (dolist (ln lines)
        (when (string-prefix-p "author " ln)
          (let ((a (substring ln 7)))
            (cl-incf total)
            (setf (alist-get a authors nil nil #'string=)
                  (1+ (or (alist-get a authors nil nil #'string=) 0))))))
      (let ((pairs (sort (mapcar (lambda (kv)
                                   (cons (car kv) (* 100.0 (/ (float (cdr kv)) (max 1 total)))))
                                 authors)
                         (lambda (a b) (> (cdr a) (cdr b))))))
        (with-current-buffer (get-buffer-create "*Git File Owners*")
          (erase-buffer)
          (insert (format "Ownership for %s (by blame lines)\n\n" (file-relative-name file)))
          (dolist (p pairs)
            (insert (format "  %-48s %5.1f%%\n" (car p) (cdr p))))
          (pop-to-buffer (current-buffer)))))))

;; ---------- Optional keys from magit-status ----------
(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c s a") #'agentic/git-top-authors)
  (define-key magit-status-mode-map (kbd "C-c s b") #'agentic/git-bus-factor)
  (define-key magit-status-mode-map (kbd "C-c s f") #'agentic/git-file-owners))

;;;###autoload

(defun agentic/gptel-use (m)
  (interactive)
  (setq gptel-model m)
  (message "gptel-model ⇒ %s" m))

;; -- Keybindings (you can change these) ----------------------------



;; -- Core keybindings (you can change these) ----------------------------

;; Keybindings: use composer instead of minibuffer for new requests



;; Minor mode keymap
(defvar agentic-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c g 5") (lambda () (interactive) (agentic/gptel-use 'gpt-5)))
    (define-key m (kbd "C-c g o") (lambda () (interactive) (agentic/gptel-use 'gpt-4o)))
    (define-key m (kbd "C-c g m") (lambda () (interactive) (agentic/gptel-use 'gpt-4o-mini)))
    (define-key m (kbd "C-c g r") #'agentic/gpt-rewrite)
    (define-key m (kbd "C-c g b") #'agentic/git-make-safe-branch)
    (define-key m (kbd "C-c g c") #'agentic/git-commit-all)
    (define-key m (kbd "C-c g u") #'agentic/git-push-current)
    (define-key m (kbd "C-c g R") #'agentic/forge-open-pr)
    (define-key m (kbd "C-c g p") #'agentic/gpt-patch-apply)
    (define-key m (kbd "C-c g P") #'agentic/gpt-patch-preview)
    (define-key m (kbd "C-c g C") #'agentic/gpt-compose-patch)
    (define-key m (kbd "C-c g W") #'agentic/gpt-compose-rewrite)
    m)
  "Keymap for `agentic-mode`.")

;;;###autoload

(define-minor-mode agentic-mode
  "Agentic LLM workflows in Emacs.
When enabled, provides keybindings under `C-c g` for agent-driven rewrites,
repo patches, and Git/Forge helpers."
  :lighter " Agentic"
  :keymap agentic-mode-map)

;;;###autoload


(define-globalized-minor-mode global-agentic-mode
  agentic-mode
  (lambda () (agentic-mode 1)))
(provide 'agentic)
;;; agentic.el ends here
